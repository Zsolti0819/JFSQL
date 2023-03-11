package com.github.jfsql.parser.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

import java.util.List;

@Getter
@ToString
@RequiredArgsConstructor
public class DeleteStatement implements DeleteWrapper {

    private final String tableName;
    private final List<String> whereColumns;
    private final List<String> whereValues;
    private final List<String> symbols;
    private final List<String> binaryOperators;

}
