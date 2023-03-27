package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.IOException;
import java.sql.SQLException;

public class NotVersioningTransactionManagerImpl extends TransactionManager {


    public NotVersioningTransactionManagerImpl(final DatabaseManager databaseManager, final Reader reader,
        final Writer writer) {
        super(databaseManager, reader, writer);
    }

    @Override
    public void commit(final String... args) throws SQLException {
        try {
            writeUncommittedObjects();
        } catch (final IOException e) {
            throw new SQLException("commit failed.\n" + e.getMessage());
        }
        removeCurrentThreadChangesFromMap();
    }

    @Override
    public void rollback() {
        throw new UnsupportedOperationException("Rollback is not possible in the not committing mode.");
    }

}
