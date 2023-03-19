package com.github.jfsql.driver.dto;

import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NonNull;

@Data
@AllArgsConstructor
public class Entry {

    @NonNull
    private Map<String, String> columnsAndValues;

    public String[] getColumns() {
        return columnsAndValues.keySet().toArray(new String[0]);
    }

    public String[] getValues() {
        return columnsAndValues.values().toArray(new String[0]);
    }
}
