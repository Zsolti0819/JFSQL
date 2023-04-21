package com.github.jfsql.parser.dto;

import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class InsertStatement implements InsertWrapper {

    private final String tableName;
    private final List<String> columns;
    private final List<List<String>> values;

}
