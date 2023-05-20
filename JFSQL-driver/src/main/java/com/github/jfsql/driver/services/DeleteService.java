package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.resultset.ResultSetCache;
import com.github.jfsql.driver.db.SharedMapHandler;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.enums.Operation;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DeleteStatement;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class DeleteService {

    private static final Logger logger = LogManager.getLogger(DeleteService.class);
    private final TransactionManager transactionManager;
    private final Database database;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    int deleteFromTable(final DeleteStatement statement) throws SQLException {
        final List<String> whereColumns = statement.getWhereColumns();
        final Table table = TableFinder.getTableByName(statement.getTableName(), database);

        if (!whereColumns.isEmpty() && (!semanticValidator.allWhereColumnsExist(table, statement))) {
            throw new SQLException("Some columns entered doesn't exist in '" + table.getName() + "'.");
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

        final List<Entry> whereEntries = WhereConditionSolver.getWhereEntries(table, statement);

        logger.debug("entries for removal = {}", whereEntries);

        final Map<String, Boolean> blobsToKeep = getBlobsToKeep(table, whereEntries);

        entries.removeAll(whereEntries);
        final int entriesSizeAfter = entries.size();
        deleteCount = entriesSizeBefore - entriesSizeAfter;

        transactionManager.execute(table, blobsToKeep, Operation.DELETE);
        return deleteCount;
    }

    private Map<String, Boolean> getBlobsToKeep(final Table table, final List<Entry> whereEntries) {
        final Map<String, Boolean> blobsToKeep = new HashMap<>();
        final List<String> blobColumns = new ArrayList<>();

        for (final Map.Entry<String, String> entry : table.getColumnsAndTypes().entrySet()) {
            final String key = entry.getKey();
            final String value = entry.getValue();
            if ("BLOB".equals(value)) {
                blobColumns.add(key);
            }
        }

        blobColumns.forEach(s -> whereEntries.forEach(entry -> {
            if (entry.getColumnsAndValues().containsKey(s)) {
                blobsToKeep.put(entry.getColumnsAndValues().get(s), false);
            }
        }));
        return blobsToKeep;
    }

}
