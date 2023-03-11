package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import lombok.RequiredArgsConstructor;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@RequiredArgsConstructor
class AlterTableService {

    private final StatementManager statementManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;
    private final Writer writer;

    void alterTable(final AlterTableWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();
        final Table table = statementManager.getTableByName(tableName);
        if (statement.getNewTableName() != null) {
            renameTable(statement, table);
        } else if (statement.getOldColumnName() != null) {
            renameColumn(statement, table);
        } else if (statement.getColumnNameToAdd() != null) {
            addColumn(statement, table);
        } else if (statement.getColumnToDrop() != null) {
            dropColumn(statement, table);
        }
        statementManager.executeDDLOperation(table);
    }

    private void renameTable(final AlterTableWrapper statement, final Table table) throws SQLException {
        final String parentDirectory = String.valueOf(statementManager.getDatabase().getUrl().getParent());
        final String newTableName = statement.getNewTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(newTableName, statementManager.getDatabase())) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final String newTableFile = parentDirectory + File.separator + newTableName + "." + writer.getFileExtension();
        final String newSchemaFile =
                writer instanceof WriterJsonImpl ? parentDirectory + File.separator + newTableName + "Schema."
                        + writer.getSchemaFileExtension()
                        : parentDirectory + File.separator + newTableName + "." + writer.getSchemaFileExtension();

        final String oldTableFile = table.getTableFile();
        final String oldSchemaFile = table.getSchemaFile();

        try {
            FileUtils.moveFile(FileUtils.getFile(oldTableFile), FileUtils.getFile(newTableFile));
            FileUtils.moveFile(FileUtils.getFile(oldSchemaFile), FileUtils.getFile(newSchemaFile));
        } catch (final IOException e) {
            throw new SQLException("Failed to rename files\n" + e.getMessage());
        }

        final String oldTableName = table.getName();

        table.setName(newTableName);
        table.setTableFile(newTableFile);
        table.setSchemaFile(newSchemaFile);
        if (table.getEntries() == null) {
            final List<Entry> entries = reader.readTable(table);
            table.setEntries(entries);
        }

        // FIXME: 3/10/2023 Test this later
        writer.getUncommittedTables().removeIf(t -> Objects.equals(t.getName(), oldTableName));
        writer.getUncommittedSchemas().removeIf(t -> Objects.equals(t.getName(), oldTableName));
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
