package com.github.jfsql.driver.validation;

public interface SchemaValidator {

    boolean schemaIsValid(final String schemaPath, final String tablePath);
}
