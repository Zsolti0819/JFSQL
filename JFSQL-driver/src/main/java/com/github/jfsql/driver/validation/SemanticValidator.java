package com.github.jfsql.driver.validation;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.StatementWithColumns;
import com.github.jfsql.parser.dto.StatementWithTableName;
import com.github.jfsql.parser.dto.StatementWithUrl;
import com.github.jfsql.parser.dto.StatementWithWhere;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
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
                final File schemaFile = new File(table.getSchemaFile());
                if (tableFile.exists() && schemaFile.exists()) {
                    return true;
                }
            }
        }

        return false;
    }

    public boolean allColumnsExist(final Table table, final StatementWithColumns statement) {
        final List<String> tableColumns = new ArrayList<>(table.getColumnsAndTypes().keySet());
        final List<String> statementColumns = statement.getColumns();
        return new HashSet<>(tableColumns).containsAll(statementColumns);
    }

    public boolean allWhereColumnsExist(final Table table, final StatementWithWhere statement) {
        final List<String> tableColumns = new ArrayList<>(table.getColumnsAndTypes().keySet());
        final List<String> statementWhereColumns = statement.getWhereColumns();
        return new HashSet<>(tableColumns).containsAll(statementWhereColumns);
    }

    public boolean isValid(final String value, final String type) {
        return Objects.equals(type, "INTEGER") && NumberUtils.isCreatable(value)
            || Objects.equals(type, "REAL") && NumberUtils.isCreatable(value)
            || Objects.equals(type, "TEXT") || Objects.equals(type, "BLOB");
    }

    public boolean statementColumnsContainDuplicates(final StatementWithColumns statement) {
        final List<String> statementColumns = statement.getColumns();
        final Set<String> columnSet = new HashSet<>(statementColumns);
        return columnSet.size() != statementColumns.size();
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

    public boolean valueCountIsLteTableColumnCount(final Table table, final InsertWrapper statement) {
        final List<List<String>> listOfValueLists = statement.getValues();
        return listOfValueLists.get(0).size() <= table.getColumnsAndTypes().size();
    }

    public boolean allInsertValuesAreValid(final Table table, final InsertWrapper statement) {
        final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
        final List<String> tableColumns = new ArrayList<>(columnsAndTypes.keySet());
        final List<String> statementColumns = statement.getColumns();
        final List<List<String>> valueLists = statement.getValues();
        final List<String> columnsToUse;

        if (statementColumns.isEmpty()) {
            columnsToUse = tableColumns;
        } else {
            columnsToUse = statementColumns;
        }

        for (final List<String> values : valueLists) {
            final LinkedHashMap<String, String> columnsAndValues = IntStream.range(0, columnsToUse.size())
                .boxed()
                .collect(Collectors.toMap(
                    columnsToUse::get, values::get,
                    (oldValue, newValue) -> oldValue,
                    LinkedHashMap::new
                ));
            for (final Map.Entry<String, String> entry : columnsAndValues.entrySet()) {
                final String column = entry.getKey();
                final String value = entry.getValue();
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
        if (Objects.equals("*", columnName)) {
            return true;
        }
        final String tableName = table.getName();
        final Map<String, String> columnAndTypes = table.getColumnsAndTypes();
        return columnAndTypes.containsKey(columnName) || columnAndTypes.containsKey(tableName + "." + columnName);
    }

    public boolean nullInsertIntoNotNullColumn(final String column, final String value, final Table table) {
        if (Objects.equals(value, null) || Objects.equals(value, "null")) {
            final Map<String, Boolean> notNullColumns = table.getNotNullColumns();
            return notNullColumns.get(column);
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
