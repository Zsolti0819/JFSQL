package com.github.jfsql.driver.exceptions;

/**
 * Custom exception thrown when the file is currently modified by another thread
 */
public class PessimisticLockException extends RuntimeException {

    public PessimisticLockException(final String message) {
        super(message);
    }

}