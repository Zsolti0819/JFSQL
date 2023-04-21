package com.github.jfsql.parser.dto;

import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class CreateTableStatement implements CreateTableWrapper {

    private final String tableName;
    private final List<String> columns;
    private final List<String> types;
    private final Map<String, Boolean> notNullColumns;
    private final boolean ifNotExistsPresent;

}
