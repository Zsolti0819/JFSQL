package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.enums.TypeOfStatement;

public interface DropTableWrapper extends BaseStatement, StatementWithTableName {

    boolean isIfExistsPresent();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.DROP_TABLE;
    }

}
