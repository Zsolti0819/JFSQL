package com.github.jfsql.driver.validation;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.*;
import org.apache.commons.lang3.math.NumberUtils;

import java.io.File;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

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
                final File schemaFile = new File(table.getSchemaFile());
                if (tableFile.exists() && schemaFile.exists()) {
                    return true;
                }
            }
        }

        return false;
    }

    public boolean allColumnsExist(final Table table, final StatementWithColumns statement) {
        return new HashSet<>(Arrays.stream(table.getColumns()).collect(Collectors.toList())).containsAll(
                statement.getColumns());
    }

    public boolean allWhereColumnsExist(final Table table, final StatementWithWhere statement) {
        return new HashSet<>(Arrays.stream(table.getColumns()).collect(Collectors.toList())).containsAll(
                statement.getWhereColumns());
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
        return listOfValueLists.get(0).size() == table.getColumns().length;
    }

    public boolean allInsertValuesAreValid(final Table activeTable, final InsertWrapper statement) {
        for (int i = 0; i < statement.getValues().size(); i++) {
            for (int j = 0; j < statement.getValues().get(i).size(); j++) {
                if (!isValid(statement.getValues().get(i).get(j), activeTable.getTypes()[j])) {
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

    public boolean columnIsPresentInTable(final String tableName, final String columnName, final Table table) {
        return Arrays.stream(table.getColumns()).anyMatch(
                s -> Objects.equals(s, columnName) ||
                        Objects.equals(s, tableName + "." + columnName));
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
