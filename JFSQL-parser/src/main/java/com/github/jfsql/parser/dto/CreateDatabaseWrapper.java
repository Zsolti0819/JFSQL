package com.github.jfsql.parser.dto;

public interface CreateDatabaseWrapper extends BaseStatement, StatementWithURL {

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.CREATE_DATABASE;
    }
}
