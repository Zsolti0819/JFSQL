package com.github.jfsql.driver.persistence;

public class PessimisticLockException extends RuntimeException {

    public PessimisticLockException(final String message) {
        super(message);
    }

}