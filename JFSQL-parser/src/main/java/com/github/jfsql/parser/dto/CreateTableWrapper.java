package com.github.jfsql.parser.dto;

import java.util.List;
import java.util.Map;

public interface CreateTableWrapper extends BaseStatement, StatementWithTableName, StatementWithColumns {

    List<String> getTypes();

    Map<String, Boolean> getNotNullColumns();

    boolean isIfNotExistsPresent();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.CREATE_TABLE;
    }
}
