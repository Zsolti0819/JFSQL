package com.github.jfsql.parser.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Getter
@ToString
@RequiredArgsConstructor
public class DropTableStatement implements DropTableWrapper {

    private final String tableName;
    private final boolean ifExistsPresent;

}
