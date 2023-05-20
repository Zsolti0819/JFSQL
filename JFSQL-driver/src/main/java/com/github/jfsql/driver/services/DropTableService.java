package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.resultset.ResultSetCache;
import com.github.jfsql.driver.db.Operation;
import com.github.jfsql.driver.db.SharedMapHandler;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropTableStatement;
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
public class DropTableService {

    private static final Logger logger = LogManager.getLogger(DropTableService.class);
    private final Database database;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    int dropTable(final DropTableStatement statement) throws SQLException {
        final boolean ifExistsIsPresent = statement.isIfExistsPresent();

        try {
            TableFinder.getTableByName(statement.getTableName(), database);
        } catch (final SQLException e) {
            final String tableName = statement.getTableName();
            if (ifExistsIsPresent) {
                logger.debug(
                    "Table '{}' does not exist, but 'IF EXISTS' clause was present in the statement, will not throw SQLException.",
                    tableName);
                return 0;
            }
        }

        final Table table = TableFinder.getTableByName(statement.getTableName(), database);

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

        if (!ifExistsIsPresent && (!semanticValidator.tableExists(statement, database))) {
            throw new SQLException(
                "Cannot DROP " + statement.getTableName() + " because the table's file or schema doesn't exist.");
        }

        SharedMapHandler.addDatabaseToSharedMap(database);
        ResultSetCache.removeResultSetFromCache(table.getName());

        final int deleteCount = entries.size();
        final List<Table> tables = database.getTables();
        tables.remove(table);

        final Map<String, Boolean> blobsToKeep = getBlobsToKeep(table, entries);

        logger.debug("table removed = {}", table);

        transactionManager.execute(table, blobsToKeep, Operation.DROP_TABLE);
        return deleteCount;
    }

    private Map<String, Boolean> getBlobsToKeep(final Table table, final List<Entry> entries) {
        final Map<String, Boolean> blobsToKeep = new HashMap<>();
        final List<String> blobColumns = new ArrayList<>();

        for (final Map.Entry<String, String> entry : table.getColumnsAndTypes().entrySet()) {
            final String key = entry.getKey();
            final String value = entry.getValue();
            if ("BLOB".equals(value)) {
                blobColumns.add(key);
            }
        }

        blobColumns.forEach(s -> entries.forEach(entry -> {
            if (entry.getColumnsAndValues().containsKey(s)) {
                blobsToKeep.put(entry.getColumnsAndValues().get(s), false);
            }
        }));
        return blobsToKeep;
    }
}
