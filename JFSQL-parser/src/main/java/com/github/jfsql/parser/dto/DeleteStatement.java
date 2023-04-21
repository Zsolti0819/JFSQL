package com.github.jfsql.parser.dto;

import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class DeleteStatement implements DeleteWrapper {

    private final String tableName;
    private final List<String> whereColumns;
    private final List<String> whereValues;
    private final List<String> symbols;
    private final List<String> binaryOperators;

}
