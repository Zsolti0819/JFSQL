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
import java.io.IOException;
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

        if (semanticValidator.statementColumnsContainDuplicates(statement)) {
            throw new SQLException("Duplicate columns were found in the statement.");
        }

        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);

        if (!semanticValidator.valueCountIsLteTableColumnCount(table, statement)) {
            throw new SQLException("The values in the parentheses were greater than the table's column count.");
        }

        if (!semanticValidator.allColumnsExist(table, statement)) {
            throw new SQLException("Some columns entered doesn't exist in '" + table.getName() + "'.");
        }

        if (!semanticValidator.allInsertValuesAreValid(table, statement)) {
            throw new SQLException(
                "Some value's type didn't match the type of the column, to which it was intended to be inserted.");
        }

        // When autoCommit is true, it should be safe to read the entries from the file
        List<Entry> entries = table.getEntries();
        if (entries == null || transactionManager.getAutoCommit()) {
            logger.trace("Will read entries from table. Table's entries were loaded into memory = {}, autoCommit = {}",
                entries != null,
                transactionManager.getAutoCommit());
            try {
                entries = reader.readEntriesFromTable(table);
            } catch (final IOException e) {
                throw new SQLException(e);
            }
            table.setEntries(entries);
        }

        final List<String> columns = statement.getColumns();
        final List<List<String>> valuesList = statement.getValues();
        for (final List<String> values : valuesList) {
            final Entry entryToInsert = getEntryToInsert(columns, values, table);
            logger.debug("entry to insert = {}", entryToInsert);
            entries.add(entryToInsert);
        }

        transactionManager.executeOperation(table);

        return valuesList.size();
    }

    Entry getEntryToInsert(final List<String> statementColumns, final List<String> values, final Table table)
        throws SQLException {
        final List<String> tableColumns = new ArrayList<>(table.getColumnsAndTypes().keySet());
        final List<String> columnsToUse = statementColumns.isEmpty() ? tableColumns : statementColumns;

        final Map<String, String> columnsAndValues = new LinkedHashMap<>();
        for (final String column : tableColumns) {
            String value = null;
            if (columnsToUse.contains(column)) {
                final int index = columnsToUse.indexOf(column);
                value = values.get(index);
            }
            if (semanticValidator.nullInsertIntoNotNullColumn(column, value, table)) {
                throw new SQLException("Inserting null value into a NOT NULL column.");
            }
            columnsAndValues.put(column, value);
        }
        final Entry entryToInsert = new Entry(columnsAndValues, new HashMap<>());

        insertBlobs(columnsToUse, table, entryToInsert);
        return entryToInsert;
    }

    private void insertBlobs(final List<String> finalColumns, final Table table, final Entry entry) {
        final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
        if (finalColumns.stream().noneMatch(column -> Objects.equals(columnsAndTypes.get(column), "BLOB"))) {
            return;
        }
        finalColumns.stream()
            .filter(column -> Objects.equals(columnsAndTypes.get(column), "BLOB"))
            .forEach(column -> {
                final LargeObject largeObject = preparedStatementCreator.getBlob(entry, column);
                if (largeObject != null) {
                    final Map<String, LargeObject> columnsAndBlobs = entry.getColumnsAndBlobs();
                    columnsAndBlobs.put(column, largeObject);
                }
            });
    }

}
