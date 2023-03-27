package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DeleteWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class DeleteService {

    private final TableFinder tableFinder;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;

    public int deleteFromTable(final DeleteWrapper statement) throws SQLException {
        final List<String> whereColumns = statement.getWhereColumns();
        final Table activeTable = tableFinder.getTableByName(statement.getTableName());

        // When autoCommit is true, it should be safe to read the entries from the file
        if (activeTable.getEntries().isEmpty() || transactionManager.getAutoCommit()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(activeTable);
                activeTable.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }

        final int deleteCount;
        final int entriesSizeBefore = activeTable.getEntries().size();
        if (whereColumns.isEmpty()) {
            activeTable.getEntries().clear();
            deleteCount = entriesSizeBefore;
        } else {
            if (!semanticValidator.allWhereColumnsExist(activeTable, statement)) {
                throw new SQLException("Some columns entered doesn't exist in '" + activeTable.getName() + "'.");
            }
            final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(activeTable, statement);
            activeTable.getEntries().removeAll(whereEntries);
            final int entriesSizeAfter = activeTable.getEntries().size();
            deleteCount = entriesSizeBefore - entriesSizeAfter;
        }

        transactionManager.executeDMLOperation(activeTable);
        return deleteCount;
    }
}
