package com.github.jfsql.driver.services;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DeleteWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class DeleteService {

    private static final Logger logger = LogManager.getLogger(DeleteService.class);
    private final TableFinder tableFinder;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;

    int deleteFromTable(final DeleteWrapper statement) throws SQLException {
        final List<String> whereColumns = statement.getWhereColumns();
        final Table table = tableFinder.getTableByName(statement.getTableName());

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

        final int deleteCount;
        final int entriesSizeBefore = entries.size();
        if (whereColumns.isEmpty()) {
            entries.clear();
            deleteCount = entriesSizeBefore;
        } else {
            if (!semanticValidator.allWhereColumnsExist(table, statement)) {
                throw new SQLException("Some columns entered doesn't exist in '" + table.getName() + "'.");
            }
            final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(table, statement);

            logger.debug("entries for removal = {}", whereEntries);

            entries.removeAll(whereEntries);
            final int entriesSizeAfter = entries.size();
            deleteCount = entriesSizeBefore - entriesSizeAfter;
        }

        transactionManager.executeDMLOperation(table);
        return deleteCount;
    }
}
