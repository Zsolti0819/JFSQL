package com.github.jfsql.driver.validation;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.StatementWithColumns;
import com.github.jfsql.parser.dto.StatementWithTableName;
import com.github.jfsql.parser.dto.StatementWithUrl;
import com.github.jfsql.parser.dto.StatementWithWhere;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.apache.commons.lang3.math.NumberUtils;

public class SemanticValidator {

    public boolean tableNameEqualsDatabaseName(final String tableName, final Database database) {
        final String databaseName = database.getUrl().getFileName().toString().replace(".json", "").replace(".xml", "");
        return Objects.equals(tableName, databaseName);
    }

    public boolean tableExists(final StatementWithTableName statement, final Database database) {
        final List<Table> tables = database.getTables();

        final String tableName = statement.getTableName();
        for (final Table table : tables) {
            if (Objects.equals(table.getName(), tableName)) {
                final File tableFile = new File(table.getTableFile());
                final File schemaFile = new File(table.getSchema().getSchemaFile());
                if (tableFile.exists() && schemaFile.exists()) {
                    return true;
                }
            }
        }

        return false;
    }

    public boolean allColumnsExist(final Table table, final StatementWithColumns statement) {
        final Map<String, String> columnsAndTypes = table.getSchema().getColumnsAndTypes();
        return new HashSet<>(columnsAndTypes.keySet()).containsAll(statement.getColumns());
    }

    public boolean allWhereColumnsExist(final Table table, final StatementWithWhere statement) {
        final Map<String, String> columnsAndTypes = table.getSchema().getColumnsAndTypes();
        return new HashSet<>(columnsAndTypes.keySet()).containsAll(statement.getWhereColumns());
    }

    public boolean isValid(final String value, final String type) {
        return Objects.equals(type, "INTEGER") && NumberUtils.isCreatable(value)
            || Objects.equals(type, "REAL") && NumberUtils.isCreatable(value)
            || Objects.equals(type, "TEXT")
            || Objects.equals(type, "BLOB");
    }

    public boolean columnsHaveDuplicate(final CreateTableWrapper statement) {
        final List<String> columns = statement.getColumns();
        final Set<String> columnSet = new HashSet<>(columns);
        return columnSet.size() != columns.size();
    }

    public boolean allInsertValuesAreEqualLength(final InsertWrapper statement) {
        final List<List<String>> listOfValueLists = statement.getValues();
        final int length = listOfValueLists.get(0).size();
        for (int i = 1; i < listOfValueLists.size(); i++) {
            if (listOfValueLists.get(i).size() != length) {
                return false;
            }
        }
        return true;
    }

    public boolean valueCountIsEqualToTableColumnCount(final Table table, final InsertWrapper statement) {
        final List<List<String>> listOfValueLists = statement.getValues();
        return listOfValueLists.get(0).size() == table.getSchema().getColumnsAndTypes().size();
    }

    public boolean allInsertValuesAreValid(final Table table, final InsertWrapper statement) {
        final List<String> statementColumns = statement.getColumns();
        final Map<String, String> columnsAndTypes = table.getSchema().getColumnsAndTypes();
        final List<String> tableColumns = new ArrayList<>(columnsAndTypes.keySet());
        final List<String> columnsToUse = statementColumns.isEmpty() ? tableColumns : statementColumns;
        for (int i = 0; i < statement.getValues().size(); i++) {
            for (int j = 0; j < statement.getValues().get(i).size(); j++) {
                final String column = columnsToUse.get(j);
                final String value = statement.getValues().get(i).get(j);
                final String type = columnsAndTypes.get(column);
                if (!isValid(value, type)) {
                    return false;
                }
            }
        }
        return true;
    }

    public boolean tableNamesAreEqual(final List<String> tableNames, final List<String> tableNamesFromJoinClause) {
        final Set<String> tableNamesSet = new HashSet<>(tableNames);
        final Set<String> tableNamesFromJoinClauseSet = new HashSet<>(tableNamesFromJoinClause);
        return Objects.equals(tableNamesSet, tableNamesFromJoinClauseSet);
    }

    public boolean columnIsPresentInTable(final Table table, final String columnName) {
        final String tableName = table.getName();
        final Schema schema = table.getSchema();
        return schema.getColumnsAndTypes().containsKey(columnName) ||
            schema.getColumnsAndTypes().containsKey(tableName + "." + columnName);
    }

    public boolean nullInsertIntoNotNullColumn(final InsertWrapper statement, final Table table) {
        final List<String> statementColumns = statement.getColumns();
        final List<List<String>> valueLists = statement.getValues();
        for (final List<String> statementValues : valueLists) {
            for (int i = 0; i < statementColumns.size(); i++) {
                final String column = statementColumns.get(i);
                final String value = statementValues.get(i);
                if ((value == null || Objects.equals(value, "null")) &&
                    Boolean.TRUE.equals(table.getSchema().getNotNullColumns().get(column))) {
                    return true;
                }
            }
        }
        return false;
    }

    public boolean urlIsAnExistingRegularFile(final StatementWithUrl statementWithUrl) {
        return Path.of(statementWithUrl.getDatabaseUrl()).toFile().isFile();
    }

    public boolean databaseExist(final StatementWithUrl statement, final String fileExtension) {
        final Path url = Path.of(statement.getDatabaseUrl());
        final String databaseFile = File.separator + url.getFileName() + "." + fileExtension;
        final Path databaseXmlFilePath = Path.of(url + databaseFile);
        return databaseXmlFilePath.toFile().exists();
    }

}
