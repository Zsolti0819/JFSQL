package com.github.jfsql.driver.dto;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import java.nio.file.Path;
import java.util.List;

@Data
@RequiredArgsConstructor
public class Database {

    private final Path url;
    @NonNull
    private List<Table> tables;

}
