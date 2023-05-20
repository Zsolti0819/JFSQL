package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.enums.TypeOfStatement;
import java.util.List;

public interface UpdateWrapper extends BaseStatement, StatementWithTableName, StatementWithWhere, StatementWithColumns {

    List<String> getValues();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.UPDATE;
    }

}

