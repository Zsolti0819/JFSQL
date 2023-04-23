package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class DropTableStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @ParameterizedTest
    @CsvSource({
        "'DROP TABLE IF EXISTS myTable;','myTable','true'",
        "'DROP TABLE myTable;','myTable','false'"
    })
    void testAlterTableStatement(final String statement, final String tableName, final String ifExistPresent) {
        final DropTableStatement dropTableStatement = (DropTableStatement) parser.parse(statement);
        assertEquals(tableName, dropTableStatement.getTableName());
        assertEquals(Boolean.valueOf(ifExistPresent), dropTableStatement.isIfExistsPresent());
    }

}