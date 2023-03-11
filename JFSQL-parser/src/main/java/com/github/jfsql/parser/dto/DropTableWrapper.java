package com.github.jfsql.parser.dto;

public interface DropTableWrapper extends BaseStatement, StatementWithTableName {

    boolean isIfExistsPresent();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.DROP_TABLE;
    }

}
