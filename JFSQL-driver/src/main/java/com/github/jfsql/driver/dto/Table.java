package com.github.jfsql.driver.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.ToString;

@Data
@AllArgsConstructor
public class Table {

    private String name;
    private String tableFile;
    @NonNull
    private Schema schema;

    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    @NonNull
    private List<Entry> entries;

}
