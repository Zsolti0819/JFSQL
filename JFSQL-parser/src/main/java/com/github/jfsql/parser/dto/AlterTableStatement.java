package com.github.jfsql.parser.dto;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Getter
@ToString
@RequiredArgsConstructor
public class AlterTableStatement implements AlterTableWrapper {

    private final String tableName;
    private final String newTableName;
    private final String oldColumnName;
    private final String newColumnName;
    private final String columnNameToAdd;
    private final String columnTypeToAdd;
    private final Boolean columnToAddCannotBeNull;
    private final String columnToDrop;

}
