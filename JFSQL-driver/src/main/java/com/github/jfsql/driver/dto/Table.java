package com.github.jfsql.driver.dto;

import java.util.List;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Generated;
import lombok.NonNull;

@Data
@AllArgsConstructor
public class Table {

    private String name;
    private String tableFile;
    private String schemaFile;
    private Map<String, String> columnsAndTypes;
    private Map<String, Boolean> notNullColumns;
    @NonNull
    private List<Entry> entries;

    public String[] getColumns() {
        return columnsAndTypes.keySet().toArray(new String[0]);
    }

    public String[] getTypes() {
        return columnsAndTypes.values().toArray(new String[0]);
    }

    @Override
    @Generated
    public String toString() {
        return "\n"
            + "Table name=" + getName() + "\n"
            + "tableFile=" + getTableFile() + "\n"
            + "schemaFile=" + getSchemaFile() + "\n"
            + "columnsAndTypes=" + getColumnsAndTypes() + "\n"
            + "notNullColumns=" + getNotNullColumns();
    }
}
