package com.github.jfsql.driver.dto;

import java.nio.file.Path;
import java.util.List;
import lombok.Data;

@Data
public class Database {

    private final Path url;
    private List<Table> tables;

    public Database(final Path url) {
        this.url = url;
    }
}
