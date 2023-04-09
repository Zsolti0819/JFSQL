package com.github.jfsql.driver.services;

import com.github.jfsql.driver.core.JfsqlResultSet;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.JoinType;
import com.github.jfsql.parser.dto.SelectWrapper;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class SelectService {

    private static final Logger logger = LogManager.getLogger(SelectService.class);
    private final TableFinder tableFinder;
    private final SemanticValidator semanticValidator;
    private final ColumnToTypeMapper columnToTypeMapper;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;

    ResultSet selectFromTable(final SelectWrapper statement) throws SQLException {
        final List<JoinType> joinTypes = statement.getJoinTypes();
        if (joinTypes.isEmpty()) {
            return simpleSelect(statement);
        }
        return selectWithJoins(statement);
    }

    private ResultSet simpleSelect(final SelectWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);
        final List<String> selectedColumns = statement.getColumns();
        for (final String columnName : selectedColumns) {
            if (!semanticValidator.columnIsPresentInTable(table, columnName)) {
                throw new SQLException("Column '" + columnName + "' not found in table '" + tableName + "'.");
            }
        }
        final List<Entry> entries;
        try {
            entries = reader.readEntriesFromTable(table);
        } catch (final IOException e) {
            throw new SQLException(e);
        }
        table.setEntries(entries);
        return baseSelect(statement, table);
    }

    private ResultSet selectWithJoins(final SelectWrapper statement) throws SQLException {
        final List<JoinType> joinTypes = statement.getJoinTypes();

        final List<Table> extractedTables = extractTables(statement);
        logger.debug("tables extracted from the statement = {}", extractedTables);

        final List<Table> modifiedTables = createModifiedTables(statement, extractedTables);
        logger.debug("tables after the modifications = {}", modifiedTables);

        final List<List<String>> listOfJoinColumns = statement.getListOfJoinColumns();

        for (int i = 0; i < joinTypes.size(); i++) {
            logger.debug("{}. loop", i);
            final Table leftTable = modifiedTables.get(0);
            final Table rightTable = modifiedTables.get(1);

            final List<String> pairedJoinColumns = pairJoinColumns(listOfJoinColumns.get(i), modifiedTables);
            logger.debug("pairedJoinColumns = {}", pairedJoinColumns);

            final List<String> modifiedJoinColumns = modifyJoinColumns(leftTable, rightTable, pairedJoinColumns);
            logger.debug("modifiedJoinColumns = {}", modifiedJoinColumns);

            final String t1JoinColumn = modifiedJoinColumns.get(0);
            final String t2JoinColumn = modifiedJoinColumns.get(1);

            final Map<String, String> mergedColumnsAndTypes = new LinkedHashMap<>(leftTable.getColumnsAndTypes());
            mergedColumnsAndTypes.putAll(rightTable.getColumnsAndTypes());
            logger.debug("mergedColumnsAndTypes = {}", mergedColumnsAndTypes);

            final Map<String, Boolean> mergedNotNullColumns = new LinkedHashMap<>(leftTable.getNotNullColumns());
            mergedNotNullColumns.putAll(rightTable.getNotNullColumns());
            logger.debug("mergedNotNullColumns = {}", mergedNotNullColumns);

            final List<Entry> entries;
            final JoinType joinType = joinTypes.get(i);
            switch (joinType) {
                case INNER_JOIN:
                    entries = innerJoin(leftTable, rightTable, t1JoinColumn, t2JoinColumn);
                    break;
                case LEFT_JOIN:
                    entries = leftJoin(leftTable, rightTable, t1JoinColumn, t2JoinColumn);
                    break;
                default:
                    throw new IllegalStateException("Unsupported join type '" + joinType + "'.");
            }

            final Table joinTable = Table.builder()
                .columnsAndTypes(mergedColumnsAndTypes)
                .notNullColumns(mergedNotNullColumns)
                .entries(entries)
                .build();

            modifiedTables.remove(leftTable);
            modifiedTables.remove(rightTable);
            modifiedTables.add(0, joinTable);
        }

        if (modifiedTables.size() != 1) {
            throw new IllegalStateException("Failed to join tables into one table.");
        }

        final Table joinTable = modifiedTables.get(0);
        return baseSelect(statement, joinTable);
    }

    private ResultSet baseSelect(final SelectWrapper statement, final Table table) throws SQLException {
        final Map<String, String> columnsAndTypes;
        List<String> selectedColumns = statement.getColumns();
        if (selectedColumns.size() == 1 && Objects.equals(selectedColumns.get(0), "*")) {
            selectedColumns = new ArrayList<>(table.getColumnsAndTypes().keySet());
            columnsAndTypes = table.getColumnsAndTypes();
        } else {
            columnsAndTypes = columnToTypeMapper.mapColumnsToTypes(statement, table);
        }

        final String tableName = statement.getTableName();
        final String tableFile = table.getTableFile();
        final String schemaFile = table.getSchemaFile();
        final Map<String, Boolean> notNullColumns = table.getNotNullColumns();
        final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(table, statement);
        List<Entry> orderedEntries = getEntriesWithSortedColumns(selectedColumns, whereEntries);

        final String limit = statement.getLimit();
        final String offset = statement.getOffset();

        orderedEntries = applyLimitAndOffset(orderedEntries, limit, offset);

        final Table newTable = Table.builder()
            .name(tableName)
            .tableFile(tableFile)
            .schemaFile(schemaFile)
            .columnsAndTypes(columnsAndTypes)
            .notNullColumns(notNullColumns)
            .entries(orderedEntries)
            .build();
        return new JfsqlResultSet(newTable, reader);
    }

    private List<Entry> getEntriesWithSortedColumns(final List<String> selectedColumns, final List<Entry> entries) {
        return entries.stream().map(entry -> {
            final Map<String, String> orderedMap = new LinkedHashMap<>();
            selectedColumns.forEach(column -> {
                final String value = entry.getColumnsAndValues().get(column);
                orderedMap.put(column, value);
            });
            return new Entry(orderedMap, new HashMap<>());
        }).collect(Collectors.toList());
    }

    private List<Entry> applyLimitAndOffset(List<Entry> orderedEntries, final String limit, final String offset)
        throws SQLException {
        if (offset != null) {
            try {
                final int offsetInteger = Integer.parseInt(offset);
                orderedEntries = orderedEntries.subList(offsetInteger, orderedEntries.size());
            } catch (final NumberFormatException e) {
                throw new SQLException("Offset '" + offset + "' is not a whole number.");
            }
        }

        if (limit != null) {
            try {
                final int limitInteger = Integer.parseInt(limit);
                orderedEntries = orderedEntries.subList(0, Math.min(limitInteger, orderedEntries.size()));
            } catch (final NumberFormatException e) {
                throw new SQLException("Limit '" + limit + "' is not a whole number.");
            }
        }
        return orderedEntries;
    }

    private List<Entry> innerJoin(final Table t1, final Table t2, final String t1JoinCol, final String t2JoinCol) {
        final List<Entry> commonEntries = new ArrayList<>();

        // create a hash table for the left table
        final Map<String, List<Entry>> hashTable = new LinkedHashMap<>();
        for (final Entry t1e : t1.getEntries()) {
            final Map<String, String> t1eColumnsAndValues = t1e.getColumnsAndValues();
            final String key = t1eColumnsAndValues.get(t1JoinCol);
            hashTable.computeIfAbsent(key, k -> new ArrayList<>());
            final List<Entry> value = hashTable.get(key);
            value.add(t1e);
        }

        // probe the right table using the hash table
        for (final Entry t2e : t2.getEntries()) {
            final Map<String, String> t2eColumnsAndValues = t2e.getColumnsAndValues();
            final String key = t2eColumnsAndValues.get(t2JoinCol);
            if (hashTable.containsKey(key)) {
                final List<Entry> matchedEntries = hashTable.get(key);
                for (final Entry t1e : matchedEntries) {
                    final Map<String, String> commonColumnsAndValues = new LinkedHashMap<>(t1e.getColumnsAndValues());
                    commonColumnsAndValues.putAll(t2eColumnsAndValues);
                    commonEntries.add(new Entry(commonColumnsAndValues, new HashMap<>()));
                }
            }
        }

        return commonEntries;
    }

    private List<Entry> leftJoin(final Table t1, final Table t2, final String t1JoinCol, final String t2JoinCol) {
        final List<Entry> joinedEntries = new ArrayList<>();

        // create a hash table for the right table
        final Map<String, List<Entry>> hashTable = new LinkedHashMap<>();
        for (final Entry t2e : t2.getEntries()) {
            final Map<String, String> t2eColumnsAndValues = t2e.getColumnsAndValues();
            final String key = t2eColumnsAndValues.get(t2JoinCol);
            hashTable.computeIfAbsent(key, k -> new ArrayList<>());
            final List<Entry> value = hashTable.get(key);
            value.add(t2e);
        }

        // probe the left table using the hash table
        for (final Entry t1e : t1.getEntries()) {
            final Map<String, String> t1eColumnsAndValues = t1e.getColumnsAndValues();
            final String key = t1eColumnsAndValues.get(t1JoinCol);
            if (hashTable.containsKey(key)) {
                final List<Entry> matchedEntries = hashTable.get(key);
                for (final Entry rightEntry : matchedEntries) {
                    final Map<String, String> joinedColumnsAndValues = new LinkedHashMap<>(t1eColumnsAndValues);
                    joinedColumnsAndValues.putAll(rightEntry.getColumnsAndValues());
                    joinedEntries.add(new Entry(joinedColumnsAndValues, new HashMap<>()));
                }
            } else {
                // add a null entry for the right table columns
                final Map<String, String> joinedColumnsAndValues = new LinkedHashMap<>(t1eColumnsAndValues);
                final Map<String, String> t2ColumnsAndTypes = t2.getColumnsAndTypes();
                for (final String columnName : t2ColumnsAndTypes.keySet()) {
                    joinedColumnsAndValues.put(columnName, null);
                }
                joinedEntries.add(new Entry(joinedColumnsAndValues, new HashMap<>()));
            }
        }

        return joinedEntries;
    }

    private List<Table> extractTables(final SelectWrapper statement) throws SQLException {
        final List<String> tableNames = new ArrayList<>();
        tableNames.add(statement.getTableName());
        tableNames.addAll(statement.getJoinTableNames());

        final List<String> tableNamesFromJoinClause = new ArrayList<>();
        final List<List<String>> listOfJoinColumns = statement.getListOfJoinColumns();

        for (final List<String> joinColumns : listOfJoinColumns) {
            joinColumns.forEach(stringFromJoinClause -> {
                final String tableName = getTableName(stringFromJoinClause);
                tableNamesFromJoinClause.add(tableName);
            });
        }

        if (!semanticValidator.tableNamesAreEqual(tableNames, tableNamesFromJoinClause)) {
            throw new SQLException("Conflicting table names were found in the statement.");
        }

        final Map<String, Table> tables = new LinkedHashMap<>();
        for (final String tableName : tableNames) {
            final Table table = tableFinder.getTableByName(tableName);
            tables.put(tableName, table);
        }

        // Check if the columns in the join clause are present in the tables
        for (final List<String> joinColumns : listOfJoinColumns) {
            for (final String joinColumn : joinColumns) {
                final String tableName = getTableName(joinColumn);
                final String columnName = getColumnName(joinColumn);
                final Table table = tables.get(tableName);
                if (!semanticValidator.columnIsPresentInTable(table, columnName)) {
                    throw new SQLException("Column '" + columnName + "' not found in table '" + tableName + "'.");
                }
            }
        }

        return new ArrayList<>(tables.values());
    }

    private List<Table> createModifiedTables(final SelectWrapper statement, final List<Table> tables)
        throws SQLException {
        final List<Table> modifiedTables = new ArrayList<>();
        final Map<String, Boolean> commonColumnsMap = getCommonColumnsMap(tables);

        // Modify column names in all tables
        for (final Table table : tables) {
            final Map<String, String> modifiedColumnsAndTypes = new LinkedHashMap<>();
            final Map<String, Boolean> modifiedNotNullColumns = new LinkedHashMap<>();

            // Modify column names in columnsAndTypes map
            final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
            for (final Map.Entry<String, String> entry : columnsAndTypes.entrySet()) {
                final String column = entry.getKey();
                final String type = entry.getValue();
                final boolean isCommonColumn = commonColumnsMap.get(column);

                final Map<String, Boolean> notNullColumns = table.getNotNullColumns();
                if (isCommonColumn) {
                    final String modifiedColumnName = table.getName() + "." + column;
                    modifiedColumnsAndTypes.put(modifiedColumnName, type);
                    modifiedNotNullColumns.put(modifiedColumnName, notNullColumns.get(column));
                } else {
                    modifiedColumnsAndTypes.put(column, type);
                    modifiedNotNullColumns.put(column, notNullColumns.get(column));
                }
            }

            // Do not set the entries at this time
            final Table modifiedTable = Table.builder()
                .name(table.getName())
                .columnsAndTypes(modifiedColumnsAndTypes)
                .notNullColumns(modifiedNotNullColumns)
                .build();
            modifiedTables.add(modifiedTable);
        }

        // Final check before loading the entries into the memory
        final Map<String, String> allColumnsAndTypes = new LinkedHashMap<>();
        for (final Table table : modifiedTables) {
            allColumnsAndTypes.putAll(table.getColumnsAndTypes());
        }
        final Table allTables = Table.builder()
            .columnsAndTypes(allColumnsAndTypes)
            .build();

        final List<String> selectedColumns = statement.getColumns();
        for (final String columnName : selectedColumns) {
            if (!semanticValidator.columnIsPresentInTable(allTables, columnName)) {
                throw new SQLException("Column '" + columnName + "' not found in the joined table.");
            }
        }

        // Now we can load the entries into memory
        for (final Table table : tables) {
            final List<Entry> entries;
            try {
                entries = reader.readEntriesFromTable(table);
            } catch (final IOException e) {
                throw new SQLException(e);
            }
            table.setEntries(entries);
        }

        for (int i = 0; i < tables.size(); i++) {
            final List<Entry> modifiedEntries = createModifiedEntries(commonColumnsMap, tables.get(i));
            final Table modifiedTable = modifiedTables.get(i);
            modifiedTable.setEntries(modifiedEntries);
        }

        return modifiedTables;
    }

    private List<Entry> createModifiedEntries(final Map<String, Boolean> commonColumnsMap, final Table table) {
        // Modify column names in entries
        final List<Entry> modifiedEntries = new ArrayList<>();
        for (final Entry entry : table.getEntries()) {
            final Map<String, String> modifiedColumnsAndValues = new LinkedHashMap<>();
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            for (final Map.Entry<String, String> columnAndValue : columnsAndValues.entrySet()) {
                final String column = columnAndValue.getKey();
                final boolean isCommonColumn = commonColumnsMap.get(column);

                if (isCommonColumn) {
                    final String modifiedColumnName = table.getName() + "." + column;
                    modifiedColumnsAndValues.put(modifiedColumnName, columnAndValue.getValue());
                } else {
                    modifiedColumnsAndValues.put(column, columnAndValue.getValue());
                }
            }
            modifiedEntries.add(new Entry(modifiedColumnsAndValues, new HashMap<>()));
        }
        return modifiedEntries;
    }

    private List<String> pairJoinColumns(final List<String> joinColumns, final List<Table> tables) {
        final List<String> pairedJoinColumns = new ArrayList<>();
        final Table table = tables.get(1);
        if (Objects.equals(table.getName(), getTableName(joinColumns.get(0)))) {
            pairedJoinColumns.add(joinColumns.get(1));
            pairedJoinColumns.add(joinColumns.get(0));
        } else {
            pairedJoinColumns.add(joinColumns.get(0));
            pairedJoinColumns.add(joinColumns.get(1));
        }
        return pairedJoinColumns;
    }

    private List<String> modifyJoinColumns(final Table t1, final Table t2, final List<String> joinColumns) {
        final Map<String, String> t1ColumnsAndTypes = t1.getColumnsAndTypes();
        final Set<String> t1Columns = t1ColumnsAndTypes.keySet();
        if (t1Columns.stream().noneMatch(joinColumns.get(0)::equals)) {
            joinColumns.set(0, getColumnName(joinColumns.get(0)));
        }

        final Map<String, String> t2ColumnsAndTypes = t2.getColumnsAndTypes();
        final Set<String> t2Columns = t2ColumnsAndTypes.keySet();
        if (t2Columns.stream().noneMatch(joinColumns.get(1)::equals)) {
            joinColumns.set(1, getColumnName(joinColumns.get(1)));
        }

        return joinColumns;
    }

    private Map<String, Boolean> getCommonColumnsMap(final List<Table> tables) {
        final Map<String, Boolean> commonColumns = new LinkedHashMap<>();
        for (final Table table : tables) {
            final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
            for (final String column : columnsAndTypes.keySet()) {
                final boolean presentInMultipleTables = commonColumns.containsKey(column);
                commonColumns.put(column, presentInMultipleTables);
            }
        }
        return commonColumns;
    }

    private String getTableName(final String columnNameWithPrefix) {
        final int dotIndex = columnNameWithPrefix.indexOf(".");
        return columnNameWithPrefix.substring(0, dotIndex);
    }

    private String getColumnName(final String columnNameWithPrefix) {
        final int dotIndex = columnNameWithPrefix.indexOf(".");
        return columnNameWithPrefix.substring(dotIndex + 1);
    }

}
