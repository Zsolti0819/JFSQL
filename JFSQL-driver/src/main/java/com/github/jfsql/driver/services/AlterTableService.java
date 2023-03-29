package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import java.io.File;
import java.io.IOException;
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
    private final IoOperationHandler ioOperationHandler;
    private final Reader reader;

    public int alterTable(final AlterTableWrapper statement) throws SQLException {
        final Database database = databaseManager.getDatabase();
        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);
        if (statement.getNewTableName() != null) {
            renameTable(statement, database, table);
        } else if (statement.getOldColumnName() != null) {
            renameColumn(statement, table);
        } else if (statement.getColumnNameToAdd() != null) {
            addColumn(statement, table);
        } else if (statement.getColumnToDrop() != null) {
            dropColumn(statement, table);
        }
        transactionManager.executeDDLOperation(database, table, table.getSchema());
        return 0;
    }

    void renameTable(final AlterTableWrapper statement, final Database database, final Table table)
        throws SQLException {

        final String newTableName = statement.getNewTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(newTableName, database)) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final String parentDirectory = String.valueOf(database.getUrl().getParent());
        final String newTableFile = parentDirectory + File.separator + newTableName + "." + reader.getFileExtension();
        final String newSchemaFile =
            reader instanceof ReaderJsonImpl ? parentDirectory + File.separator + newTableName + "Schema."
                + reader.getSchemaFileExtension()
                : parentDirectory + File.separator + newTableName + "." + reader.getSchemaFileExtension();

        final String oldTableFile = table.getTableFile();
        final String oldSchemaFile = table.getSchema().getSchemaFile();

        try {
            ioOperationHandler.renameFile(oldTableFile, newTableFile);
            ioOperationHandler.renameFile(oldSchemaFile, newSchemaFile);
        } catch (final IOException e) {
            if (transactionManager.getUncommittedTables().contains(table)) {
                logger.debug(
                    "The table has not yet been written to file, but it is present in the list of uncommitted tables.");
            } else {
                throw new SQLException("Failed to rename files.\n" + e.getMessage());
            }
        }

        table.setName(newTableName);
        table.setTableFile(newTableFile);
        table.getSchema().setSchemaFile(newSchemaFile);
        if (table.getEntries().isEmpty()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(table);
                table.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }
    }

    private void renameColumn(final AlterTableWrapper statement, final Table table) throws SQLException {

        if (table.getSchema().getColumnsAndTypes().containsKey(statement.getNewColumnName())) {
            throw new SQLException(
                "The column '" + statement.getNewColumnName() + "' already exists in '" + table.getName() + "'");
        }

        final Map<String, String> modifiedColumnsAndTypes = getModifiedColumnsAndTypes(statement, table);
        table.getSchema().setColumnsAndTypes(modifiedColumnsAndTypes);

        final Map<String, Boolean> modifiedNotNullColumns = getModifiedNotNullColumns(statement, table);
        table.getSchema().setNotNullColumns(modifiedNotNullColumns);

        if (table.getEntries().isEmpty()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(table);
                table.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }

        // Modify the column name for every entry in the table
        for (final Entry entry : table.getEntries()) {
            final LinkedHashMap<String, String> modifiedColumnsAndValues = new LinkedHashMap<>();
            for (final Map.Entry<String, String> columnValuePair : entry.getColumnsAndValues().entrySet()) {
                if (Objects.equals(columnValuePair.getKey(), statement.getOldColumnName())) {
                    modifiedColumnsAndValues.put(statement.getNewColumnName(), columnValuePair.getValue());
                } else {
                    modifiedColumnsAndValues.put(columnValuePair.getKey(), columnValuePair.getValue());
                }
            }
            entry.setColumnsAndValues(modifiedColumnsAndValues);
        }
    }

    private Map<String, Boolean> getModifiedNotNullColumns(final AlterTableWrapper statement, final Table table) {
        final LinkedHashMap<String, Boolean> modifiedNotNullColumns = new LinkedHashMap<>();
        for (final Map.Entry<String, Boolean> notNullColumnPair : table.getSchema().getNotNullColumns().entrySet()) {
            if (Objects.equals(notNullColumnPair.getKey(), statement.getOldColumnName())) {
                modifiedNotNullColumns.put(statement.getNewColumnName(), notNullColumnPair.getValue());
            } else {
                modifiedNotNullColumns.put(notNullColumnPair.getKey(), notNullColumnPair.getValue());
            }
        }
        return modifiedNotNullColumns;
    }

    private Map<String, String> getModifiedColumnsAndTypes(final AlterTableWrapper statement, final Table table) {
        final LinkedHashMap<String, String> modifiedColumnsAndTypes = new LinkedHashMap<>();
        for (final Map.Entry<String, String> columnTypePair : table.getSchema().getColumnsAndTypes().entrySet()) {
            if (Objects.equals(columnTypePair.getKey(), statement.getOldColumnName())) {
                modifiedColumnsAndTypes.put(statement.getNewColumnName(), columnTypePair.getValue());
            } else {
                modifiedColumnsAndTypes.put(columnTypePair.getKey(), columnTypePair.getValue());
            }
        }
        return modifiedColumnsAndTypes;
    }

    private void addColumn(final AlterTableWrapper statement, final Table table) throws SQLException {
        final String columnNameToAdd = statement.getColumnNameToAdd();
        final String columnTypeToAdd = statement.getColumnTypeToAdd();

        if (table.getSchema().getColumnsAndTypes().containsKey(columnNameToAdd)) {
            throw new SQLException("The column '" + columnNameToAdd + "' already exists in '" + table.getName() + "'.");
        }

        table.getSchema().getColumnsAndTypes().put(columnNameToAdd, columnTypeToAdd);
        if (Boolean.TRUE.equals(statement.getColumnToAddCannotBeNull())) {
            table.getSchema().getNotNullColumns().put(columnNameToAdd, true);
        } else if (Boolean.FALSE.equals(statement.getColumnToAddCannotBeNull())) {
            table.getSchema().getNotNullColumns().put(columnNameToAdd, false);
        }

        if (table.getEntries().isEmpty()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(table);
                table.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }

        // Add the column to every entry in the table
        for (final Entry entry : table.getEntries()) {
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
                        throw new IllegalStateException("Unknown data type '" + columnTypeToAdd + "'");
                }
                entry.getColumnsAndValues().put(columnNameToAdd, valueToAdd);
            } else if (Boolean.FALSE.equals(statement.getColumnToAddCannotBeNull())) {
                entry.getColumnsAndValues().put(columnNameToAdd, null);
            }
        }
    }

    private void dropColumn(final AlterTableWrapper statement, final Table table) throws SQLException {
        if (!table.getSchema().getColumnsAndTypes().containsKey(statement.getColumnToDrop())) {
            throw new SQLException(
                "The column '" + statement.getColumnToDrop() + "' doesn't exist in '" + table.getName() + "'");
        }

        table.getSchema().getColumnsAndTypes().remove(statement.getColumnToDrop());
        table.getSchema().getNotNullColumns().remove(statement.getColumnToDrop());

        if (table.getEntries().isEmpty()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(table);
                table.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }

        // Remove the columns from every entry in the table
        for (final Entry entry : table.getEntries()) {
            entry.getColumnsAndValues().remove(statement.getColumnToDrop());
        }
    }
}
