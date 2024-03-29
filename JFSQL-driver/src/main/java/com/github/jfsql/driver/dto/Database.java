package com.github.jfsql.driver.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NonNull;

@Data
@AllArgsConstructor
public class Database {

    private final String name;
    private final String URL;
    @NonNull
    private List<Table> tables;

}
