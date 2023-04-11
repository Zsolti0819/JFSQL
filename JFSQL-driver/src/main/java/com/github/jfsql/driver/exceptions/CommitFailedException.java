package com.github.jfsql.driver.exceptions;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;

/**
 * Custom exception created in order to be able to catch failed commit in functions
 * {@link TransactionManager#executeOperation(Table)}, {@link TransactionManager#executeOperation(Database)}
 * {@link TransactionManager#executeOperation(Database, Table)},
 */
public class CommitFailedException extends RuntimeException {

    public CommitFailedException(final String message) {
        super(message);
    }

}