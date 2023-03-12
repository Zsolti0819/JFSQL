package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.InsertWrapper;
import lombok.RequiredArgsConstructor;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

@RequiredArgsConstructor
public class InsertService {

    private final StatementManager statementManager;
    private final Transaction transaction;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    public int insertIntoTable(final InsertWrapper statement) throws SQLException {
        if (!semanticValidator.allInsertValuesAreEqualLength(statement)) {
            throw new SQLException("The values in some parentheses were not equal.");
        }

        final String tableName = statement.getTableName();
        final Table activeTable = statementManager.getTableByName(tableName);

        if (!semanticValidator.valueCountIsEqualToTableColumnCount(activeTable, statement)) {
            throw new SQLException(
                    "The values in the parentheses were lower or greater than the table's column count.");
        }

        if (!semanticValidator.allColumnsExist(activeTable, statement)) {
            throw new SQLException("Some columns entered doesn't exist in \"" + activeTable.getName() + "\".");
        }

        if (!semanticValidator.allInsertValuesAreValid(activeTable, statement)) {
            throw new SQLException(
                    "Some value's type didn't match the type of the column, to which it was intended to be inserted.");
        }

        if (activeTable.getEntries() == null) {
            final List<Entry> entries = reader.readTable(activeTable);
            activeTable.setEntries(entries);
        }

        final List<Entry> entriesToInsert = getEntriesToInsert(statement, activeTable);
        activeTable.getEntries().addAll(entriesToInsert);

        transaction.executeDMLOperation(activeTable);

        return statement.getValues().size();
    }

    private List<Entry> getEntriesToInsert(final InsertWrapper insertStatement, final Table activeTable) {
        final List<Entry> insertEntries = new ArrayList<>();
        for (int i = 0; i < insertStatement.getValues().size(); i++) {
            final Map<String, String> columnsAndValues = new LinkedHashMap<>();
            final int finalI = i;
            if (insertStatement.getColumns().isEmpty()) {
                IntStream.range(0, activeTable.getColumns().length)
                        .forEach(j -> columnsAndValues.put(activeTable.getColumns()[j],
                                insertStatement.getValues().get(finalI).get(j)));
            } else {
                IntStream.range(0, insertStatement.getColumns().size())
                        .forEach(j -> columnsAndValues.put(insertStatement.getColumns().get(j),
                                insertStatement.getValues().get(finalI).get(j)));
            }
            insertEntries.add(new Entry(columnsAndValues));
        }
        return insertEntries;
    }
}
