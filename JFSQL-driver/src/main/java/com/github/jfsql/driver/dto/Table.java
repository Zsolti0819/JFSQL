package com.github.jfsql.driver.dto;

import java.util.List;
import java.util.Map;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

@Data
@Builder
public class Table {

    private String name;
    private String tableFile;
    private String schemaFile;
    private Map<String, String> columnsAndTypes;
    private Map<String, Boolean> notNullColumns;
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private List<Entry> entries;

}
