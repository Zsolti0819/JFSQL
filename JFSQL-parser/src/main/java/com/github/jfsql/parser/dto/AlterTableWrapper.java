package com.github.jfsql.parser.dto;

public interface AlterTableWrapper extends BaseStatement, StatementWithTableName {

    String getNewTableName();

    String getOldColumnName();

    String getNewColumnName();

    String getColumnNameToAdd();

    String getColumnTypeToAdd();

    Boolean getColumnToAddCannotBeNull();

    String getColumnToDrop();

    @Override
    default TypeOfStatement getTypeOfStatement() {
        return TypeOfStatement.ALTER_TABLE;
    }
}
