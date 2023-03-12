package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;

import java.nio.file.Path;
import java.sql.SQLException;

public class NotCommittingTransaction extends Transaction {


    public NotCommittingTransaction(final Path url, final Reader reader, final Writer writer) throws SQLException {
        super(url, reader, writer);
    }

    @Override
    public void commit() throws SQLException {
        if (!autoCommit) {
            writeUncommittedObjects();
        }
    }

    @Override
    public void rollback() {
        throw new UnsupportedOperationException("Rollback is not possible in the not committing mode.");
    }
}
