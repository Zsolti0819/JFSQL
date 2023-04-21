package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.resultset.ResultSetCache;
import com.github.jfsql.driver.db.SharedMapHandler;
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

        if (!whereColumns.isEmpty()) {
            if (!semanticValidator.allWhereColumnsExist(table, statement)) {
                throw new SQLException("Some columns entered doesn't exist in '" + table.getName() + "'.");
            }
        }

        SharedMapHandler.addTableToSharedMap(table);
        ResultSetCache.removeResultSetFromCache(table.getName());

        List<Entry> entries = table.getEntries();
        if (entries == null) {
            try {
                entries = reader.readEntriesFromTable(table);
            } catch (final IOException e) {
                SharedMapHandler.removeCurrentThreadChangesFromMap();
                throw new SQLException(e);
            }
            table.setEntries(entries);
        }

        final int deleteCount;
        final int entriesSizeBefore = entries.size();

        final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(table, statement);

        logger.debug("entries for removal = {}", whereEntries);

        entries.removeAll(whereEntries);
        final int entriesSizeAfter = entries.size();
        deleteCount = entriesSizeBefore - entriesSizeAfter;

        transactionManager.executeOperation(table, false);
        return deleteCount;
    }

}
