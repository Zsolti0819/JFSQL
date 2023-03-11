package com.github.jfsql.driver.transactions;

import lombok.experimental.SuperBuilder;

import java.sql.SQLException;

@SuperBuilder
public class NotCommittingTransaction extends Transaction {

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
