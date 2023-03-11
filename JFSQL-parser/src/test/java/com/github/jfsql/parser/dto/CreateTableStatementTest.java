package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.core.Parser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CreateTableStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @ParameterizedTest
    @CsvSource({
            "'create table myTable (a integer not null, b real, c text not null, d blob)','myTable','a','integer','b','real','c','text','d','blob'",
    })
    void testCreateTableStatements(final String statement, final String tableName, final String column1Name, final String column1Type, final String column2Name, final String column2Type, final String column3Name, final String column3Type, final String column4Name, final String column4Type) {
        final CreateTableStatement createTableStatement = (CreateTableStatement) parser.parse(statement);
        assertEquals(tableName, createTableStatement.getTableName());
        assertEquals(column1Name, createTableStatement.getColumns().get(0));
        assertEquals(column1Type, createTableStatement.getTypes().get(0));
        assertEquals(column2Name, createTableStatement.getColumns().get(1));
        assertEquals(column2Type, createTableStatement.getTypes().get(1));
        assertEquals(column3Name, createTableStatement.getColumns().get(2));
        assertEquals(column3Type, createTableStatement.getTypes().get(2));
        assertEquals(column4Name, createTableStatement.getColumns().get(3));
        assertEquals(column4Type, createTableStatement.getTypes().get(3));
        assertTrue(createTableStatement.getColumns().getClass().getName().contains("Unmodifiable"));
        assertTrue(createTableStatement.getTypes().getClass().getName().contains("Unmodifiable"));
    }

}