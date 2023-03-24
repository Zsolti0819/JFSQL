package com.github.jfsql.driver.persistence;

public class PessimisticLockException extends RuntimeException {

    public PessimisticLockException(String message) {
        super(message);
    }

}