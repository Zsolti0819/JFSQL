package com.github.jfsql.driver.dto;

import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NonNull;

@Data
@AllArgsConstructor
public class Schema {

    private String schemaFile;
    @NonNull
    private Map<String, String> columnsAndTypes;
    @NonNull
    private Map<String, Boolean> notNullColumns;

}
