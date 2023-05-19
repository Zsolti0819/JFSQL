package com.github.jfsql.parser.dto;

import java.util.List;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DeleteStatement implements DeleteWrapper {

    private final String tableName;
    private final List<String> whereColumns;
    private final List<String> whereValues;
    private final List<String> symbols;
    private final List<String> binaryOperators;

}
