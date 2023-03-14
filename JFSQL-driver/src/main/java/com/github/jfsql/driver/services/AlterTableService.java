package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.transactions.TransactionManager;
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
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
class AlterTableService {

    private static final Logger logger = LogManager.getLogger(AlterTableService.class);
    private final TableFinder tableFinder;
    private final Database database;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    void alterTable(final AlterTableWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);
        if (statement.getNewTableName() != null) {
            renameTable(statement, table);
        } else if (statement.getOldColumnName() != null) {
            renameColumn(statement, table);
        } else if (statement.getColumnNameToAdd() != null) {
            addColumn(statement, table);
        } else if (statement.getColumnToDrop() != null) {
            dropColumn(statement, table);
        }
        transactionManager.executeDDLOperation(table);
    }

    private void renameTable(final AlterTableWrapper statement, final Table table) throws SQLException {
        final String parentDirectory = String.valueOf(database.getUrl().getParent());
        final String newTableName = statement.getNewTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(newTableName, database)) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final String newTableFile = parentDirectory + File.separator + newTableName + "." + reader.getFileExtension();
        final String newSchemaFile =
            reader instanceof ReaderJsonImpl ? parentDirectory + File.separator + newTableName + "Schema."
                + reader.getSchemaFileExtension()
                : parentDirectory + File.separator + newTableName + "." + reader.getSchemaFileExtension();

        final String oldTableFile = table.getTableFile();
        final String oldSchemaFile = table.getSchemaFile();

        try {
            FileUtils.moveFile(FileUtils.getFile(oldTableFile), FileUtils.getFile(newTableFile));
            FileUtils.moveFile(FileUtils.getFile(oldSchemaFile), FileUtils.getFile(newSchemaFile));
        } catch (final IOException e) {
            if (!transactionManager.getAutoCommit() && transactionManager.getUncommittedTables().contains(table)) {
                logger.debug(
                    "The table has not yet been written to files, but is present in the list of uncommitted tables.");
            } else {
                throw new SQLException("Failed to rename files\n" + e.getMessage());
            }
        }

        table.setName(newTableName);
        table.setTableFile(newTableFile);
        table.setSchemaFile(newSchemaFile);
        if (table.getEntries() == null) {
            final List<Entry> entries = reader.readTable(table);
            table.setEntries(entries);
        }
    }

    private void renameColumn(final AlterTableWrapper statement, final Table table) throws SQLException {

        if (table.getColumnsAndTypes().containsKey(statement.getNewColumnName())) {
            throw new SQLException(
                "The column '" + statement.getNewColumnName() + "' already exists in '" + table.getName() + "'");
        }

        final Map<String, String> modifiedColumnsAndTypes = getModifiedColumnsAndTypes(statement, table);
        table.setColumnsAndTypes(modifiedColumnsAndTypes);

        final Map<String, Boolean> modifiedNotNullColumns = getModifiedNotNullColumns(statement, table);
        table.setNotNullColumns(modifiedNotNullColumns);

        if (table.getEntries() == null) {
            final List<Entry> entries = reader.readTable(table);
            table.setEntries(entries);
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
        for (final Map.Entry<String, Boolean> notNullColumnPair : table.getNotNullColumns().entrySet()) {
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
        for (final Map.Entry<String, String> columnTypePair : table.getColumnsAndTypes().entrySet()) {
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

        if (table.getColumnsAndTypes().containsKey(columnNameToAdd)) {
            throw new SQLException("The column '" + columnNameToAdd + "' already exists in '" + table.getName() + "'");
        }

        table.getColumnsAndTypes().put(columnNameToAdd, columnTypeToAdd);
        if (Boolean.TRUE.equals(statement.getColumnToAddCannotBeNull())) {
            table.getNotNullColumns().put(columnNameToAdd, true);
        } else if (Boolean.FALSE.equals(statement.getColumnToAddCannotBeNull())) {
            table.getNotNullColumns().put(columnNameToAdd, false);
        }

        if (table.getEntries() == null) {
            final List<Entry> entries = reader.readTable(table);
            table.setEntries(entries);
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
        if (!table.getColumnsAndTypes().containsKey(statement.getColumnToDrop())) {
            throw new SQLException(
                "The column '" + statement.getColumnToDrop() + "' doesn't exist in '" + table.getName() + "'");
        }

        table.getColumnsAndTypes().remove(statement.getColumnToDrop());
        table.getNotNullColumns().remove(statement.getColumnToDrop());

        if (table.getEntries() == null) {
            final List<Entry> entries = reader.readTable(table);
            table.setEntries(entries);
        }

        // Remove the columns from every entry in the table
        for (final Entry entry : table.getEntries()) {
            entry.getColumnsAndValues().remove(statement.getColumnToDrop());
        }
    }
}
