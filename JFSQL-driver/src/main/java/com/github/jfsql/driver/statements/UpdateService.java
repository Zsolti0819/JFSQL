package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.UpdateWrapper;
import lombok.RequiredArgsConstructor;

import java.sql.SQLException;
import java.util.List;

@RequiredArgsConstructor
public class UpdateService {

    private final StatementManager statementManager;
    private final Transaction transaction;
    private final SemanticValidator semanticValidator;
    private final ColumnToTypeMapper columnToTypeMapper;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;

    public int updateTable(final UpdateWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();
        final Table activeTable = statementManager.getTableByName(tableName);

        if (!semanticValidator.allColumnsExist(activeTable, statement)) {
            throw new SQLException("Some columns entered doesn't exist in \"" + activeTable.getName() + "\".");
        }

        if (!semanticValidator.allWhereColumnsExist(activeTable, statement)) {
            throw new SQLException("Some columns entered doesn't exist in \"" + activeTable.getName() + "\".");
        }

        final String[] types = columnToTypeMapper.mapColumnsToTypes(statement, activeTable).values()
                .toArray(new String[0]);

        if (activeTable.getEntries() == null) {
            final List<Entry> entries = reader.readTable(activeTable);
            activeTable.setEntries(entries);
        }

        final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(activeTable, statement);

        final List<String> columns = statement.getColumns();
        final List<String> values = statement.getValues();

        for (final Entry entry : whereEntries) {
            for (int i = 0; i < columns.size(); i++) {
                if (semanticValidator.isValid(values.get(i), types[i])) {
                    entry.getColumnsAndValues().put(columns.get(i), values.get(i));
                }
            }
        }

        transaction.executeDMLOperation(activeTable);

        return whereEntries.size();
    }
}
