package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
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

    public int updateTable(final UpdateWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();
        final Table activeTable = tableFinder.getTableByName(tableName);

        if (!semanticValidator.allColumnsExist(activeTable, statement)) {
            throw new SQLException("Some columns entered doesn't exist in \"" + activeTable.getName() + "\".");
        }

        if (!semanticValidator.allWhereColumnsExist(activeTable, statement)) {
            throw new SQLException("Some columns entered doesn't exist in \"" + activeTable.getName() + "\".");
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

        final List<Entry> whereEntries = whereConditionSolver.getWhereEntries(activeTable, statement);

        logger.debug("whereEntries = {}", whereEntries);

        final List<String> columns = statement.getColumns();
        final List<String> values = statement.getValues();
        final List<String> types = new ArrayList<>(
            columnToTypeMapper.mapColumnsToTypes(statement, activeTable).values());

        for (final Entry entry : whereEntries) {
            for (int i = 0; i < columns.size(); i++) {
                if (semanticValidator.isValid(values.get(i), types.get(i))) {
                    entry.getColumnsAndValues().put(columns.get(i), values.get(i));
                } else {
                    throw new SQLException(
                        "Not valid update. Value '" + values.get(i) + "' cannot be converted to '" + types.get(i)
                            + "'.");
                }
            }
        }

        transactionManager.executeDMLOperation(activeTable);

        return whereEntries.size();
    }
}
