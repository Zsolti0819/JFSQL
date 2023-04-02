package com.github.jfsql.driver.dto;

import java.util.Map;
import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class Entry {

    @NonNull
    private Map<String, String> columnsAndValues;
    @NonNull
    private Map<String, LargeObject> columnsAndBlobs;

}
