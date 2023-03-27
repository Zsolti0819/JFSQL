package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.InsertWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
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

    public int insertIntoTable(final InsertWrapper statement) throws SQLException {
        if (!semanticValidator.allInsertValuesAreEqualLength(statement)) {
            throw new SQLException("The values in some parentheses were not equal.");
        }

        final String tableName = statement.getTableName();
        final Table activeTable = tableFinder.getTableByName(tableName);

        if (!semanticValidator.valueCountIsEqualToTableColumnCount(activeTable, statement)) {
            throw new SQLException(
                "The values in the parentheses were lower or greater than the table's column count.");
        }

        if (!semanticValidator.allColumnsExist(activeTable, statement)) {
            throw new SQLException("Some columns entered doesn't exist in '" + activeTable.getName() + "'.");
        }

        if (!semanticValidator.allInsertValuesAreValid(activeTable, statement)) {
            throw new SQLException(
                "Some value's type didn't match the type of the column, to which it was intended to be inserted.");
        }

        // When autoCommit is true, it should be safe to read the entries from the file
        if (activeTable.getEntries().isEmpty() || transactionManager.getAutoCommit()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(activeTable);
                activeTable.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }

        final List<Entry> entriesToInsert = getEntriesToInsert(statement, activeTable);
        logger.debug("entriesToInsert = {}", entriesToInsert);

        activeTable.getEntries().addAll(entriesToInsert);

        transactionManager.executeDMLOperation(activeTable);

        return statement.getValues().size();
    }

    private List<Entry> getEntriesToInsert(final InsertWrapper insertStatement, final Table activeTable) {
        final List<String> tableColumns = new ArrayList<>(activeTable.getSchema().getColumnsAndTypes().keySet());
        final List<String> statementColumns = insertStatement.getColumns();
        final List<Entry> insertEntries = new ArrayList<>();
        for (int i = 0; i < insertStatement.getValues().size(); i++) {
            final Map<String, String> columnsAndValues = new LinkedHashMap<>();
            final int finalI = i;
            if (insertStatement.getColumns().isEmpty()) {
                IntStream.range(0, activeTable.getSchema().getColumnsAndTypes().size())
                    .forEach(j -> columnsAndValues.put(tableColumns.get(j),
                        insertStatement.getValues().get(finalI).get(j)));
            } else {
                for (final String columnName : tableColumns) {
                    if (statementColumns.contains(columnName)) {
                        final int index = statementColumns.indexOf(columnName);
                        final String value = insertStatement.getValues().get(i).get(index);
                        columnsAndValues.put(columnName, value);
                    } else {
                        throw new IllegalStateException(
                            "The column '" + columnName + "' was not present in the statement.");
                    }
                }
            }
            insertEntries.add(new Entry(columnsAndValues));
        }
        return insertEntries;
    }

}
