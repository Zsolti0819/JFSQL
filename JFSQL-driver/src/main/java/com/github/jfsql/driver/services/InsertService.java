package com.github.jfsql.driver.services;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.PreparedStatementCreator;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.InsertWrapper;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class InsertService {

    private static final Logger logger = LogManager.getLogger(InsertService.class);
    private final TableFinder tableFinder;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;
    private final PreparedStatementCreator preparedStatementCreator;

    int insertIntoTable(final InsertWrapper statement) throws SQLException {
        if (!semanticValidator.allInsertValuesAreEqualLength(statement)) {
            throw new SQLException("The values in some parentheses were not equal.");
        }

        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);

        if (!semanticValidator.valueCountIsEqualToTableColumnCount(table, statement)) {
            throw new SQLException(
                "The values in the parentheses were lower or greater than the table's column count.");
        }

        if (!semanticValidator.allColumnsExist(table, statement)) {
            throw new SQLException("Some columns entered doesn't exist in '" + table.getName() + "'.");
        }

        if (!semanticValidator.allInsertValuesAreValid(table, statement)) {
            throw new SQLException(
                "Some value's type didn't match the type of the column, to which it was intended to be inserted.");
        }

        if (semanticValidator.nullInsertIntoNotNullColumn(statement, table)) {
            throw new SQLException("Inserting null value into a NOT NULL column.");
        }

        // When autoCommit is true, it should be safe to read the entries from the file
        if (table.getEntries() == null || transactionManager.getAutoCommit()) {
            logger.debug("table.getEntries() == null ? {}", table.getEntries() == null);
            final List<Entry> entries = reader.readEntriesFromTable(table);
            table.setEntries(entries);
        }

        List<String> finalColumns = statement.getColumns();
        if (finalColumns.isEmpty()) {
            finalColumns = new ArrayList<>(table.getColumnsAndTypes().keySet());
        }

        final List<Entry> entriesToInsert = getEntriesToInsert(finalColumns, statement, table);
        table.getEntries().addAll(entriesToInsert);

        transactionManager.executeDMLOperation(table);

        return statement.getValues().size();
    }

    private List<Entry> getEntriesToInsert(final List<String> finalColumns, final InsertWrapper statement,
        final Table table) {
        final List<String> tableColumns = new ArrayList<>(table.getColumnsAndTypes().keySet());
        final List<List<String>> insertValueLists = statement.getValues();
        final List<Entry> insertEntries = new ArrayList<>();
        for (final List<String> insertValueList : insertValueLists) {
            final Map<String, String> columnsAndValues = new LinkedHashMap<>();
            for (final String columnName : tableColumns) {
                if (finalColumns.contains(columnName)) {
                    final int index = finalColumns.indexOf(columnName);
                    final String value = insertValueList.get(index);
                    columnsAndValues.put(columnName, value);
                } else {
                    throw new IllegalStateException(
                        "The column '" + columnName + "' was not present in the statement.");
                }
            }
            insertEntries.add(new Entry(columnsAndValues, new HashMap<>()));
        }

        insertBlobs(finalColumns, table, insertEntries);

        logger.debug("insertEntries = {}", insertEntries);
        return insertEntries;
    }

    private void insertBlobs(final List<String> finalColumns, final Table table, final List<Entry> insertEntries) {
        if (finalColumns.stream().noneMatch(s -> Objects.equals(table.getColumnsAndTypes().get(s), "BLOB"))) {
            return;
        }
        finalColumns.stream()
            .filter(s -> Objects.equals(table.getColumnsAndTypes().get(s), "BLOB"))
            .forEach(s -> {
                for (final Entry entry : insertEntries) {
                    final LargeObject largeObject = preparedStatementCreator.getBlob(entry, s);
                    if (largeObject != null) {
                        entry.getColumnsAndBlobs().put(s, largeObject);
                    }
                }
            });
    }

}
