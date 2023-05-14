package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.resultset.ResultSetCache;
import com.github.jfsql.driver.db.Operation;
import com.github.jfsql.driver.db.SharedMapHandler;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.PreparedStatementCreator;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class UpdateService {

    private static final Logger logger = LogManager.getLogger(UpdateService.class);
    private final TableFinder tableFinder;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final ColumnToTypeMapper columnToTypeMapper;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;
    private final PreparedStatementCreator preparedStatementCreator;

    int updateTable(final UpdateWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();
        final Table table = tableFinder.getTableByName(tableName);

        if (!semanticValidator.allColumnsExist(table, statement)) {
            throw new SQLException("Some columns entered doesn't exist in '" + table.getName() + "'.");
        }

        if (!semanticValidator.allWhereColumnsExist(table, statement)) {
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

        final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(table, statement);
        logger.debug("entries to update = {}", whereEntries);

        final List<String> columns = statement.getColumns();
        final List<String> values = statement.getValues();
        final Map<String, String> columnsMappedToTypes = columnToTypeMapper.mapColumnsToTypes(statement, table);
        final List<String> types = new ArrayList<>(columnsMappedToTypes.values());

        for (final Entry entry : whereEntries) {
            for (int i = 0; i < columns.size(); i++) {
                final String column = columns.get(i);
                final String value = values.get(i);
                final String type = types.get(i);
                updateEntry(entry, column, value, type);
            }
        }

        final Map<String, Boolean> blobsToKeep = getBlobsToKeep(table, whereEntries);

        transactionManager.execute(table, blobsToKeep, Operation.UPDATE);
        return whereEntries.size();
    }

    private void updateEntry(final Entry entry, final String column, final String value, final String type)
        throws SQLException {
        logger.debug("entry before the update = {}", entry);
        if (semanticValidator.isValid(value, type)) {
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            columnsAndValues.put(column, value);
        } else {
            SharedMapHandler.removeCurrentThreadChangesFromMap();
            throw new SQLException("Value " + value + " cannot be converted to '" + type + "'.");
        }
        if (Objects.equals(type, "BLOB")) {
            final LargeObject largeObject = preparedStatementCreator.getBlob(entry, column);
            if (largeObject != null) {
                final Map<String, LargeObject> columnsAndBlobs = entry.getColumnsAndBlobs();
                columnsAndBlobs.put(column, largeObject);
            }
        }
        logger.debug("entry after the update = {}", entry);
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
                blobsToKeep.put(entry.getColumnsAndValues().get(s), true);
            }
        }));
        return blobsToKeep;
    }
}
