package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropTableWrapper;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.SQLException;
import java.util.List;

@RequiredArgsConstructor
public class DropTableService {

    private final StatementManager statementManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;
    private static final Logger logger = LogManager.getLogger(DropTableService.class);

    public int dropTable(final DropTableWrapper statement) throws SQLException {
        final boolean ifExistsIsPresent = statement.isIfExistsPresent();

        try {
            statementManager.getTableByName(statement.getTableName());
        } catch (final SQLException e) {
            final String tableName = statement.getTableName();
            if (ifExistsIsPresent) {
                logger.info("Table '{}' does not exist, but 'IF EXISTS' clause was present in the statement, will not throw SQLException.", tableName);
                return 0;
            }
        }

        final Table activeTable = statementManager.getTableByName(statement.getTableName());
        if (activeTable.getEntries() == null) {
            final List<Entry> entries = reader.readTable(activeTable);
            activeTable.setEntries(entries);
        }

        if (!ifExistsIsPresent && (!semanticValidator.tableExists(statement, statementManager.getDatabase()))) {
            throw new SQLException("Cannot DROP " + statement.getTableName() + " because the table's file or schema doesn't exist.");
        }

        final int deleteCount = activeTable.getEntries().size();
        statementManager.getDatabase().getTables().remove(activeTable);
        statementManager.getDatabase().setActiveTable(null);
        statementManager.executeDropTableOperation();
        return deleteCount;
    }
}
