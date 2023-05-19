package com.github.jfsql.parser.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
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
