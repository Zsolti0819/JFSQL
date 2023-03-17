package com.github.jfsql.driver.dto;

import java.nio.file.Path;
import java.util.List;
import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class Database {

    private final Path url;
    @NonNull
    private List<Table> tables;

}
