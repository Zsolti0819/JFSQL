package com.github.jfsql.parser.dto;

import java.util.List;

public interface SelectWrapper extends BaseStatement, StatementWithTableName, StatementWithWhere, StatementWithColumns {

    List<String> getJoinTableNames();

    List<JoinType> getJoinTypes();

    List<List<String>> getListOfJoinColumns();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.SELECT;
    }

}
