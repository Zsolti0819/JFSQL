package com.github.jfsql.parser.dto;

import java.util.List;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class SelectStatement implements SelectWrapper {

    private final String tableName;
    private final List<String> joinTableNames;
    private final List<JoinType> joinTypes;
    private final List<String> columns;
    private final List<List<String>> listOfJoinColumns;
    private final List<String> whereColumns;
    private final List<String> whereValues;
    private final List<String> symbols;
    private final List<String> binaryOperators;
    private final String orderColumn;
    private final OrderBy orderBy;
    private final String limit;
    private final String offset;

}
