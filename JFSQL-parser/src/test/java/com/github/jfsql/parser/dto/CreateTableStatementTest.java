package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CreateTableStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @Test
    void testCreateTableStatement() {
        final String statement = "CREATE TABLE myTable (a INTEGER NOT NULL, b REAL, c TEXT NOT NULL, d BLOB);";
        final CreateTableStatement createTableStatement = (CreateTableStatement) parser.parse(statement);
        assertEquals("myTable", createTableStatement.getTableName());
        assertEquals("a", createTableStatement.getColumns().get(0));
        assertEquals("INTEGER", createTableStatement.getTypes().get(0));
        assertEquals("b", createTableStatement.getColumns().get(1));
        assertEquals("REAL", createTableStatement.getTypes().get(1));
        assertEquals("c", createTableStatement.getColumns().get(2));
        assertEquals("TEXT", createTableStatement.getTypes().get(2));
        assertEquals("d", createTableStatement.getColumns().get(3));
        assertEquals("BLOB", createTableStatement.getTypes().get(3));
    }
}