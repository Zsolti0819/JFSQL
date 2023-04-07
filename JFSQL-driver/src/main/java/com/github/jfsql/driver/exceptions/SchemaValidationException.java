package com.github.jfsql.driver.exceptions;

/**
 * Custom exception thrown when a table is not valid against the schema. Both xsd and json schema variations use this
 * exception
 */
public class SchemaValidationException extends RuntimeException {

    public SchemaValidationException(final String message) {
        super(message);
    }
}
