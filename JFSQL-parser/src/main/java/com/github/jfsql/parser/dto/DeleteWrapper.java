package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.enums.TypeOfStatement;

public interface DeleteWrapper extends BaseStatement, StatementWithWhere, StatementWithTableName {

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.DELETE;
    }
}
