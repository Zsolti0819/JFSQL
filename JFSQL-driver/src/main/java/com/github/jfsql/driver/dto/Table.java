package com.github.jfsql.driver.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Generated;
import lombok.NonNull;

@Data
@AllArgsConstructor
public class Table {

    private String name;
    private String tableFile;
    @NonNull
    private Schema schema;
    @NonNull
    private List<Entry> entries;

    @Override
    @Generated
    public String toString() {
        return "\n"
            + "Table name=" + getName() + "\n"
            + "tableFile=" + getTableFile() + "\n"
            + "schemaFile=" + schema.getSchemaFile() + "\n"
            + "columnsAndTypes=" + schema.getColumnsAndTypes() + "\n"
            + "notNullColumns=" + schema.getNotNullColumns();
    }
}
