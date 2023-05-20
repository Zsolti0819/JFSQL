package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.enums.TypeOfStatement;
import java.util.List;

public interface InsertWrapper extends BaseStatement, StatementWithTableName, StatementWithColumns {

    List<List<String>> getValues();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.INSERT;
    }

}
