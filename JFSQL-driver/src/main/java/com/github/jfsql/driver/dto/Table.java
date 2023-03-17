package com.github.jfsql.driver.dto;

import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.Generated;

@Data
public class Table {

    private String name;
    private String tableFile;
    private String schemaFile;
    private Map<String, String> columnsAndTypes;
    private Map<String, Boolean> notNullColumns;
    private List<Entry> entries;

    public Table(final String name, final String tableFile, final String schemaFile,
        final Map<String, String> columnsAndTypes, final Map<String, Boolean> notNullColumns) {
        this.name = name;
        this.tableFile = tableFile;
        this.schemaFile = schemaFile;
        this.columnsAndTypes = columnsAndTypes;
        this.notNullColumns = notNullColumns;
    }

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
