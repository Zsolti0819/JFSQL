package com.github.jfsql.parser.dto;

import java.util.List;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class InsertStatement implements InsertWrapper {

    private final String tableName;
    private final List<String> columns;
    private final List<List<String>> values;

}
