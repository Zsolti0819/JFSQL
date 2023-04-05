package com.github.jfsql.parser.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Getter
@ToString
@RequiredArgsConstructor
public class DropDatabaseStatement implements DropDatabaseWrapper {

    private final String databaseURL;

}
