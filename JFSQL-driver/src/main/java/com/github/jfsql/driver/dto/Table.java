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

    public String[] getColumns() {
        return schema.getColumnsAndTypes().keySet().toArray(new String[0]);
    }

    public String[] getTypes() {
        return schema.getColumnsAndTypes().values().toArray(new String[0]);
    }

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
