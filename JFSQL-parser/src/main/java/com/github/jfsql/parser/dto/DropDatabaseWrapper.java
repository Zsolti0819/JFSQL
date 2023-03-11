package com.github.jfsql.parser.dto;

public interface DropDatabaseWrapper extends BaseStatement, StatementWithUrl {

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.DROP_DATABASE;
    }
}
