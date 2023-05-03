package com.github.jfsql.driver.exceptions;

/**
 * Custom exception created in order to be able to catch failed commit
 */
public class CommitFailedException extends RuntimeException {

    public CommitFailedException(final Throwable throwable) {
        super(throwable);
    }

}