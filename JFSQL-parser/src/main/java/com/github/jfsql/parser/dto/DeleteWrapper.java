package com.github.jfsql.parser.dto;

public interface DeleteWrapper extends BaseStatement, StatementWithWhere, StatementWithTableName {

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.DELETE;
    }
}
