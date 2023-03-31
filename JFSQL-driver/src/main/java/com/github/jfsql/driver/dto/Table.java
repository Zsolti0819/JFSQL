package com.github.jfsql.driver.dto;

import java.util.ArrayList;
import java.util.LinkedHashMap;
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
    @Builder.Default
    private Map<String, String> columnsAndTypes = new LinkedHashMap<>();
    @Builder.Default
    private Map<String, Boolean> notNullColumns = new LinkedHashMap<>();
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @Builder.Default
    private List<Entry> entries = new ArrayList<>();

}
