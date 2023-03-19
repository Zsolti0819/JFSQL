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

}
