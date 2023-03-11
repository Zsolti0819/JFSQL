package com.github.jfsql.parser.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Getter
@ToString
@RequiredArgsConstructor
public class CreateDatabaseStatement implements CreateDatabaseWrapper {

    private final String databaseUrl;

}
