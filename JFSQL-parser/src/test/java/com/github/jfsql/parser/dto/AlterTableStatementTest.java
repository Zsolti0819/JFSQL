package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class AlterTableStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @ParameterizedTest
    @CsvSource({
        "'ALTER TABLE myTable RENAME COLUMN myColumn TO newMyColumn;','myTable',,'myColumn','newMyColumn',,,",
        "'ALTER TABLE myTable RENAME TO myNewTable;','myTable',myNewTable,,,,,",
        "'ALTER TABLE myTable ADD COLUMN myNewColumn INTEGER NOT NULL ;','myTable',,,,'myNewColumn','INTEGER',",
        "'ALTER TABLE myTable DROP COLUMN columnToDrop;','myTable',,,,,,'columnToDrop'",
    })
    void testAlterTableStatements(final String statement, final String tableName, final String newTableName,
        final String oldColumnName, final String newColumnName, final String columnNameToAdd,
        final String columnTypeToAdd, final String columnToDrop) {
        final AlterTableStatement alterTableStatement = (AlterTableStatement) parser.parse(statement);
        assertEquals(tableName, alterTableStatement.getTableName());
        assertEquals(newTableName, alterTableStatement.getNewTableName());
        assertEquals(oldColumnName, alterTableStatement.getOldColumnName());
        assertEquals(newColumnName, alterTableStatement.getNewColumnName());
        assertEquals(columnNameToAdd, alterTableStatement.getColumnNameToAdd());
        assertEquals(columnTypeToAdd, alterTableStatement.getColumnTypeToAdd());
        assertEquals(columnToDrop, alterTableStatement.getColumnToDrop());
    }
}