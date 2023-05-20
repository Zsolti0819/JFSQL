package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.enums.JoinType;
import com.github.jfsql.parser.enums.OrderBy;
import com.github.jfsql.parser.enums.TypeOfStatement;
import java.util.List;

public interface SelectWrapper extends BaseStatement, StatementWithTableName, StatementWithWhere, StatementWithColumns {

    List<String> getJoinTableNames();

    List<JoinType> getJoinTypes();

    List<List<String>> getListOfJoinColumns();

    String getOrderColumn();

    OrderBy getOrderBy();

    String getLimit();

    String getOffset();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.SELECT;
    }

}
