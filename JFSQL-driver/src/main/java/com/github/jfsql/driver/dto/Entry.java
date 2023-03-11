package com.github.jfsql.driver.dto;

import java.util.Map;
import lombok.Data;

@Data
public class Entry {

    private Map<String, String> columnsAndValues;

    public Entry(final Map<String, String> columnsAndValues) {
        this.columnsAndValues = columnsAndValues;
    }

    public String[] getColumns() {
        return columnsAndValues.keySet().toArray(new String[0]);
    }

    public String[] getValues() {
        return columnsAndValues.values().toArray(new String[0]);
    }
}
