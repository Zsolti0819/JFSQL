package com.github.jfsql.parser.dto;

import java.util.List;

public interface InsertWrapper extends BaseStatement, StatementWithTableName, StatementWithColumns {

    List<List<String>> getValues();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.INSERT;
    }

}
