package com.github.jfsql.parser.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

import java.util.List;

@Getter
@ToString
@RequiredArgsConstructor
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

}
