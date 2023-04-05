package com.github.jfsql.driver.dto;

import java.nio.file.Path;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NonNull;

@Data
@AllArgsConstructor
public class Database {

    private final Path URL;
    @NonNull
    private List<Table> tables;

}
