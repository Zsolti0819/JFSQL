package com.github.jfsql.driver.services;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class AlterTableService {

    private static final Logger logger = LogManager.getLogger(AlterTableService.class);
    private final TableFinder tableFinder;
    private final DatabaseManager databaseManager;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final FileNameCreator fileNameCreator;
    private final Reader reader;

    int alterTable(final AlterTableWrapper statement) throws SQLException {
        final Database database = databaseManager.getDatabase();
        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);
        if (statement.getNewTableName() != null) {
            renameTable(statement, database, table);
        } else if (statement.getOldColumnName() != null) {
            renameColumn(statement, database, table);
        } else if (statement.getColumnNameToAdd() != null) {
            addColumn(statement, database, table);
        } else if (statement.getColumnToDrop() != null) {
            dropColumn(statement, database, table);
        }
        return 0;
    }

    void renameTable(final AlterTableWrapper statement, final Database database, final Table table)
        throws SQLException {

        final String newTableName = statement.getNewTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(newTableName, database)) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final String newTableFile = fileNameCreator.createTableFileName(newTableName, database);
        final String newSchemaFile = fileNameCreator.createSchemaFileName(newTableName, database);

        final List<Entry> entries = reader.readEntriesFromTable(table);
        table.setEntries(entries);

        table.setName(newTableName);
        table.setTableFile(newTableFile);
        table.setSchemaFile(newSchemaFile);

        transactionManager.executeDDLOperation(database, table);
    }

    void renameColumn(final AlterTableWrapper statement, final Database database, final Table table)
        throws SQLException {

        final String newColumnName = statement.getNewColumnName();

        if (semanticValidator.columnIsPresentInTable(table, newColumnName)) {
            throw new SQLException("The column '" + newColumnName + "' already exists in '" + table.getName() + "'");
        }

        // When autoCommit is true, it should be safe to read the entries from the file
        List<Entry> entries = table.getEntries();
        if (entries == null || transactionManager.getAutoCommit()) {
            logger.debug("Will read entries from table. Table's entries were loaded into memory = {}, autoCommit = {}",
                entries != null,
                transactionManager.getAutoCommit());
            entries = reader.readEntriesFromTable(table);
            table.setEntries(entries);
        } else {
            logger.debug("Will not read entries from table. autoCommit = {}", transactionManager.getAutoCommit());
        }

        final Map<String, String> modifiedColumnsAndTypes = getModifiedColumnsAndTypes(statement, table);
        table.setColumnsAndTypes(modifiedColumnsAndTypes);
        final Map<String, Boolean> modifiedNotNullColumns = getModifiedNotNullColumns(statement, table);
        table.setNotNullColumns(modifiedNotNullColumns);

        // Modify the column name for every entry in the table
        for (final Entry entry : table.getEntries()) {
            final LinkedHashMap<String, String> modifiedColumnsAndValues = new LinkedHashMap<>();
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            for (final Map.Entry<String, String> columnValuePair : columnsAndValues.entrySet()) {
                if (Objects.equals(columnValuePair.getKey(), statement.getOldColumnName())) {
                    modifiedColumnsAndValues.put(newColumnName, columnValuePair.getValue());
                } else {
                    modifiedColumnsAndValues.put(columnValuePair.getKey(), columnValuePair.getValue());
                }
            }
            entry.setColumnsAndValues(modifiedColumnsAndValues);
        }
        transactionManager.executeDDLOperation(database, table);
    }

    private Map<String, Boolean> getModifiedNotNullColumns(final AlterTableWrapper statement, final Table table) {
        final LinkedHashMap<String, Boolean> modifiedNotNullColumns = new LinkedHashMap<>();
        final String newColumnName = statement.getNewColumnName();
        final Map<String, Boolean> notNullColumns = table.getNotNullColumns();
        for (final Map.Entry<String, Boolean> notNullColumnPair : notNullColumns.entrySet()) {
            if (Objects.equals(notNullColumnPair.getKey(), statement.getOldColumnName())) {
                modifiedNotNullColumns.put(newColumnName, notNullColumnPair.getValue());
            } else {
                modifiedNotNullColumns.put(notNullColumnPair.getKey(), notNullColumnPair.getValue());
            }
        }
        return modifiedNotNullColumns;
    }

    private Map<String, String> getModifiedColumnsAndTypes(final AlterTableWrapper statement, final Table table) {
        final LinkedHashMap<String, String> modifiedColumnsAndTypes = new LinkedHashMap<>();
        final String newColumnName = statement.getNewColumnName();
        final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
        for (final Map.Entry<String, String> columnTypePair : columnsAndTypes.entrySet()) {
            if (Objects.equals(columnTypePair.getKey(), statement.getOldColumnName())) {
                modifiedColumnsAndTypes.put(newColumnName, columnTypePair.getValue());
            } else {
                modifiedColumnsAndTypes.put(columnTypePair.getKey(), columnTypePair.getValue());
            }
        }
        return modifiedColumnsAndTypes;
    }

    void addColumn(final AlterTableWrapper statement, final Database database, final Table table) throws SQLException {
        final String columnNameToAdd = statement.getColumnNameToAdd();
        final String columnTypeToAdd = statement.getColumnTypeToAdd();

        final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
        final Map<String, Boolean> notNullColumns = table.getNotNullColumns();

        if (semanticValidator.columnIsPresentInTable(table, columnNameToAdd)) {
            throw new SQLException("The column '" + columnNameToAdd + "' already exists in '" + table.getName() + "'.");
        }

        // When autoCommit is true, it should be safe to read the entries from the file
        List<Entry> entries = table.getEntries();
        if (entries == null || transactionManager.getAutoCommit()) {
            logger.debug("Will read entries from table. Table's entries were loaded into memory = {}, autoCommit = {}",
                entries != null,
                transactionManager.getAutoCommit());
            entries = reader.readEntriesFromTable(table);
            table.setEntries(entries);
        } else {
            logger.debug("Will not read entries from table. autoCommit = {}", transactionManager.getAutoCommit());
        }

        columnsAndTypes.put(columnNameToAdd, columnTypeToAdd);
        if (Boolean.TRUE.equals(statement.getColumnToAddCannotBeNull())) {
            notNullColumns.put(columnNameToAdd, true);
        } else if (Boolean.FALSE.equals(statement.getColumnToAddCannotBeNull())) {
            notNullColumns.put(columnNameToAdd, false);
        }

        // Add the column to every entry in the table
        for (final Entry entry : table.getEntries()) {
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            if (Boolean.TRUE.equals(statement.getColumnToAddCannotBeNull())) {
                final String valueToAdd;
                switch (columnTypeToAdd) {
                    case "INTEGER":
                    case "REAL":
                        valueToAdd = "0";
                        break;
                    case "TEXT":
                    case "BLOB":
                        valueToAdd = StringUtils.EMPTY;
                        break;
                    default:
                        throw new IllegalStateException("Unsupported data type '" + columnTypeToAdd + "'");
                }
                columnsAndValues.put(columnNameToAdd, valueToAdd);
            } else {
                columnsAndValues.put(columnNameToAdd, null);
            }
        }
        transactionManager.executeDDLOperation(database, table);
    }

    void dropColumn(final AlterTableWrapper statement, final Database database, final Table table) throws SQLException {
        final String columnNameToDrop = statement.getColumnToDrop();

        if (!semanticValidator.columnIsPresentInTable(table, columnNameToDrop)) {
            throw new SQLException("The column '" + columnNameToDrop + "' doesn't exist in '" + table.getName() + "'");
        }

        final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
        columnsAndTypes.remove(columnNameToDrop);
        final Map<String, Boolean> notNullColumns = table.getNotNullColumns();
        notNullColumns.remove(columnNameToDrop);

        // When autoCommit is true, it should be safe to read the entries from the file
        List<Entry> entries = table.getEntries();
        if (entries == null || transactionManager.getAutoCommit()) {
            logger.debug("Will read entries from table. Table's entries were loaded into memory = {}, autoCommit = {}",
                entries != null,
                transactionManager.getAutoCommit());
            entries = reader.readEntriesFromTable(table);
            table.setEntries(entries);
        } else {
            logger.debug("Will not read entries from table. autoCommit = {}", transactionManager.getAutoCommit());
        }

        // Remove the columns from every entry in the table
        for (final Entry entry : table.getEntries()) {
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            columnsAndValues.remove(columnNameToDrop);
        }
        transactionManager.executeDDLOperation(database, table);
    }
}
