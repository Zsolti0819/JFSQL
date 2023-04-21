package com.github.jfsql.parser.dto;

import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class DropTableStatement implements DropTableWrapper {

    private final String tableName;
    private final boolean ifExistsPresent;

}
