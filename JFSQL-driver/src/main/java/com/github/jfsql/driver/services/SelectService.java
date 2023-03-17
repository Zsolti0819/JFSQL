package com.github.jfsql.driver.services;

import com.github.jfsql.driver.core.JfsqlResultSet;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.JoinType;
import com.github.jfsql.parser.dto.SelectWrapper;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
class SelectService {

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

        for (final JoinType joinType : joinTypes) {
            if (!Objects.equals(joinType, JoinType.INNER_JOIN) && !Objects.equals(joinType, JoinType.LEFT_JOIN)) {
                throw new IllegalStateException("Unsupported join type.\n" + statement);
            }
        }

        final List<Table> extractedTables = extractTables(statement);
        // Now we can load the entries into memory
        for (final Table table : extractedTables) {
            if (table.getEntries() == null) {
                final List<Entry> entries = reader.readTable(table);
                table.setEntries(entries);
            }
        }
        logger.debug("tables extracted from the statement = {}", extractedTables);
        final List<Table> modifiedTables = createModifiedTables(extractedTables);
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

            final Map<String, String> mergedColumnsAndTypes = new LinkedHashMap<>(leftTable.getColumnsAndTypes());
            mergedColumnsAndTypes.putAll(rightTable.getColumnsAndTypes());
            logger.debug("mergedColumnsAndTypes = {}", mergedColumnsAndTypes);

            final Map<String, Boolean> mergedNotNullColumns = new LinkedHashMap<>(leftTable.getNotNullColumns());
            mergedNotNullColumns.putAll(rightTable.getNotNullColumns());
            logger.debug("mergedNotNullColumns = {}", mergedNotNullColumns);

            Table joinTable = null;
            if (Objects.equals(joinTypes.get(i), JoinType.INNER_JOIN)) {
                joinTable = innerJoin(leftTable, rightTable, mergedColumnsAndTypes, mergedNotNullColumns,
                    modifiedJoinColumns);
                logger.debug("table created by inner join = {}", joinTable);
            } else if (Objects.equals(joinTypes.get(i), JoinType.LEFT_JOIN)) {
                joinTable = leftJoin(leftTable, rightTable, mergedColumnsAndTypes, mergedNotNullColumns,
                    modifiedJoinColumns);
                logger.debug("table created by left join = {}", joinTable);
            }
            modifiedTables.remove(leftTable);
            modifiedTables.remove(rightTable);
            modifiedTables.add(0, joinTable);
        }

        return baseSelect(statement, modifiedTables.get(0));

    }

    private ResultSet baseSelect(final SelectWrapper statement, final Table table) {
        final Map<String, String> columnsAndTypes;

        List<String> selectedColumns = statement.getColumns();
        if (selectedColumns.size() == 1 && Objects.equals(selectedColumns.get(0), "*")) {
            selectedColumns = List.of(table.getColumns());
            columnsAndTypes = table.getColumnsAndTypes();
        } else {
            columnsAndTypes = columnToTypeMapper.mapColumnsToTypes(statement, table);
        }

        final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(table, statement);

        final String tableFile = table.getTableFile();
        final String schemaFile = table.getSchemaFile();
        final Map<String, Boolean> notNullColumns = table.getNotNullColumns();

        final Table newTable = new Table(null, tableFile, schemaFile, columnsAndTypes, notNullColumns);
        final List<Entry> orderedEntries = getEntriesWithSortedColumns(selectedColumns, whereEntries);
        newTable.setEntries(orderedEntries);

        return new JfsqlResultSet(statement, newTable);
    }

    private ResultSet simpleSelect(final SelectWrapper statement) throws SQLException {
        final Table activeTable = tableFinder.getTableByName(statement.getTableName());
        if (activeTable.getEntries() == null) {
            final List<Entry> entries = reader.readTable(activeTable);
            activeTable.setEntries(entries);
        }
        return baseSelect(statement, activeTable);
    }

    private Table innerJoin(final Table firstTable, final Table secondTable,
        final Map<String, String> mergedColumnsAndTypes, final Map<String, Boolean> mergedNotNullColumns,
        final List<String> joinColumns) {
        final String t1JoinColumn = joinColumns.get(0);
        final String t2JoinColumn = joinColumns.get(1);
        final Table joinTable = new Table("joinTable", null, null, mergedColumnsAndTypes, mergedNotNullColumns);
        final List<Entry> commonEntries = new ArrayList<>();

        for (final Entry t1e : firstTable.getEntries()) {
            for (final Entry t2e : secondTable.getEntries()) {
                if (Objects.equals(t1e.getColumnsAndValues().get(t1JoinColumn),
                    t2e.getColumnsAndValues().get(t2JoinColumn))) {
                    final Map<String, String> commonColumnsAndValues = new LinkedHashMap<>(t1e.getColumnsAndValues());
                    commonColumnsAndValues.putAll(t2e.getColumnsAndValues());
                    commonEntries.add(new Entry(commonColumnsAndValues));
                }
            }
        }
        joinTable.setEntries(commonEntries);
        return joinTable;
    }

    private Table leftJoin(final Table firstTable, final Table secondTable,
        final Map<String, String> mergedColumnsAndTypes, final Map<String, Boolean> mergedNotNullColumns,
        final List<String> joinColumns) {
        final String t1JoinColumn = joinColumns.get(0);
        final String t2JoinColumn = joinColumns.get(1);
        final Table joinTable = new Table("joinTable", null, null, mergedColumnsAndTypes, mergedNotNullColumns);
        final List<Entry> resultEntries = new ArrayList<>();

        for (final Entry t1e : firstTable.getEntries()) {
            boolean t1eMatched = false;
            for (final Entry t2e : secondTable.getEntries()) {
                if (Objects.equals(t1e.getColumnsAndValues().get(t1JoinColumn),
                    t2e.getColumnsAndValues().get(t2JoinColumn))) {
                    t1eMatched = true;
                    final Map<String, String> mergedValues = new LinkedHashMap<>(t1e.getColumnsAndValues());
                    for (final Map.Entry<String, String> t2ColumnAndValue : t2e.getColumnsAndValues().entrySet()) {
                        final String column = t2ColumnAndValue.getKey();
                        final String value = t2ColumnAndValue.getValue();
                        mergedValues.putIfAbsent(column, value);
                    }
                    resultEntries.add(new Entry(mergedValues));
                }
            }
            if (!t1eMatched) {
                final Map<String, String> mergedValues = new LinkedHashMap<>(t1e.getColumnsAndValues());
                for (final Map.Entry<String, String> t2ColumnAndValue : secondTable.getEntries().get(0)
                    .getColumnsAndValues().entrySet()) {
                    final String column = t2ColumnAndValue.getKey();
                    mergedValues.putIfAbsent(column, null);
                }
                resultEntries.add(new Entry(mergedValues));
            }
        }

        joinTable.setEntries(resultEntries);
        return joinTable;
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
                if (!semanticValidator.columnIsPresentInTable(tableName, columnName, table)) {
                    throw new SQLException("Column '" + columnName + "' not found in table '" + tableName + "'");
                }
            }
        }

        return new ArrayList<>(tables.values());
    }

    private List<Table> createModifiedTables(final List<Table> tables) {
        final List<Table> modifiedTables = new ArrayList<>();
        final Map<String, Set<String>> commonColumns = findCommonColumnsAndMapToTables(tables);
        final Map<String, String> tableNameMap = new HashMap<>();

        // Modify column names in all tables
        for (final Table table : tables) {
            final Map<String, String> modifiedColumnsAndTypes = new LinkedHashMap<>();
            final Map<String, Boolean> modifiedNotNullColumns = new LinkedHashMap<>();

            // Modify column names in columnsAndTypes map
            for (final Map.Entry<String, String> entry : table.getColumnsAndTypes().entrySet()) {
                final String column = entry.getKey();
                final Set<String> tableNames = commonColumns.get(column);

                if (tableNames != null && tableNames.size() > 1) {
                    // Column found in multiple tables
                    final String modifiedColumnName = table.getName() + "." + column;
                    modifiedColumnsAndTypes.put(modifiedColumnName, entry.getValue());
                    modifiedNotNullColumns.put(modifiedColumnName, table.getNotNullColumns().get(column));
                    tableNameMap.put(column, table.getName());
                } else {
                    // Column unique to this table
                    modifiedColumnsAndTypes.put(column, entry.getValue());
                    modifiedNotNullColumns.put(column, table.getNotNullColumns().get(column));
                }
            }

            final List<Entry> modifiedEntries = createModifiedEntries(tableNameMap, table);

            final Table modifiedTable = new Table(table.getName(), null, null, modifiedColumnsAndTypes,
                modifiedNotNullColumns);
            modifiedTable.setEntries(modifiedEntries);
            modifiedTables.add(modifiedTable);
        }
        return modifiedTables;
    }

    private List<Entry> createModifiedEntries(final Map<String, String> tableNameMap, final Table table) {
        // Modify column names in entries
        final List<Entry> modifiedEntries = new ArrayList<>();
        for (final Entry entry : table.getEntries()) {
            final Map<String, String> modifiedColumnsAndValues = new LinkedHashMap<>();
            for (final Map.Entry<String, String> columnAndValue : entry.getColumnsAndValues().entrySet()) {
                final String column = columnAndValue.getKey();
                final String tableName = tableNameMap.get(column);

                if (tableName != null) {
                    final String modifiedColumnName = tableName + "." + column;
                    modifiedColumnsAndValues.put(modifiedColumnName, columnAndValue.getValue());
                } else {
                    modifiedColumnsAndValues.put(column, columnAndValue.getValue());
                }
            }
            modifiedEntries.add(new Entry(modifiedColumnsAndValues));
        }
        return modifiedEntries;
    }

    private List<String> pairJoinColumns(final List<String> joinColumns, final List<Table> tables) {
        final List<String> pairedJoinColumns = new ArrayList<>();
        if (Objects.equals(tables.get(1).getName(), getTableName(joinColumns.get(0)))) {
            pairedJoinColumns.add(joinColumns.get(1));
            pairedJoinColumns.add(joinColumns.get(0));
        } else {
            pairedJoinColumns.add(joinColumns.get(0));
            pairedJoinColumns.add(joinColumns.get(1));
        }
        return pairedJoinColumns;
    }

    private List<String> modifyJoinColumns(final Table firstTable, final Table secondTable,
        final List<String> joinColumns) {
        if (Arrays.stream(firstTable.getColumns()).noneMatch(joinColumns.get(0)::equals)) {
            joinColumns.set(0, getColumnName(joinColumns.get(0)));
        }

        if (Arrays.stream(secondTable.getColumns()).noneMatch(joinColumns.get(1)::equals)) {
            joinColumns.set(1, getColumnName(joinColumns.get(1)));
        }

        return joinColumns;
    }

    private Map<String, Set<String>> findCommonColumnsAndMapToTables(final List<Table> tables) {
        final Map<String, Set<String>> commonColumns = new LinkedHashMap<>();
        // Find common columns and create table name mapping
        for (final Table table : tables) {
            final String tableName = table.getName();
            for (final String column : table.getColumns()) {
                if (commonColumns.containsKey(column)) {
                    // Column already found in another table
                    commonColumns.get(column).add(tableName);
                } else {
                    // First occurrence of column
                    commonColumns.put(column, new LinkedHashSet<>(Collections.singletonList(tableName)));
                }
            }
        }
        return commonColumns;
    }

    private List<Entry> getEntriesWithSortedColumns(final List<String> selectedColumns, final List<Entry> entries) {
        return entries.stream().map(entry -> {
            final Map<String, String> orderedMap = new LinkedHashMap<>();
            final String[] columns = entry.getColumns();
            final String[] values = entry.getValues();
            selectedColumns.forEach(selectedColumn -> {
                for (int i = 0; i < columns.length; i++) {
                    if (Objects.equals(columns[i], selectedColumn)) {
                        orderedMap.put(columns[i], values[i]);
                    }
                }
            });
            return new Entry(orderedMap);
        }).collect(Collectors.toList());
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