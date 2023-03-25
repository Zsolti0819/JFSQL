package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropTableWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class DropTableService {

    private static final Logger logger = LogManager.getLogger(DropTableService.class);
    private final TableFinder tableFinder;
    private final DatabaseManager databaseManager;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    public int dropTable(final DropTableWrapper statement) throws SQLException {
        final Database database = databaseManager.getDatabase();
        final boolean ifExistsIsPresent = statement.isIfExistsPresent();

        try {
            tableFinder.getTableByName(statement.getTableName());
        } catch (final SQLException e) {
            final String tableName = statement.getTableName();
            if (ifExistsIsPresent) {
                logger.debug(
                    "Table '{}' does not exist, but 'IF EXISTS' clause was present in the statement, will not throw SQLException.",
                    tableName);
                return 0;
            }
        }

        final Table activeTable = tableFinder.getTableByName(statement.getTableName());
        if (activeTable.getEntries().isEmpty()) {
            try {
                final List<Entry> entries = reader.readEntriesFromTable(activeTable);
                activeTable.setEntries(entries);
            } catch (final IOException e) {
                throw new SQLException("Failed to read entries from the table.\n" + e.getMessage());
            }
        }

        if (!ifExistsIsPresent && (!semanticValidator.tableExists(statement, database))) {
            throw new SQLException(
                "Cannot DROP " + statement.getTableName() + " because the table's file or schema doesn't exist.");
        }

        final int deleteCount = activeTable.getEntries().size();
        database.getTables().remove(activeTable);
        transactionManager.executeDropTableOperation();
        return deleteCount;
    }
}
