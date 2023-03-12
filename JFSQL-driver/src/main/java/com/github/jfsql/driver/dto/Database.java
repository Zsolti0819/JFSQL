package com.github.jfsql.driver.dto;

import java.nio.file.Path;
import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class Database {

    private final Path url;
    private List<Table> tables;

}
