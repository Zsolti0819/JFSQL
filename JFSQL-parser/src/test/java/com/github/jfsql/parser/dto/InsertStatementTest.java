package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class InsertStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @Test
    void testInsertStatement() {
        final String statement = "INSERT INTO myTable (column1, column2, column3) VALUES (1, 2, 3);";
        final InsertStatement insertStatement = (InsertStatement) parser.parse(statement);
        assertEquals("myTable", insertStatement.getTableName());
        assertEquals(List.of("column1", "column2", "column3"), insertStatement.getColumns());
        assertEquals(List.of(List.of("1", "2", "3")), insertStatement.getValues());

    }

    @Test
    void testInsertStatement_multiRow() {
        final String statement = "INSERT INTO myTable (column1, column2, column3) VALUES (1, 2, 3), (4, 5, 6), (7, 8, 9);";
        final InsertStatement insertStatement = (InsertStatement) parser.parse(statement);
        assertEquals("myTable", insertStatement.getTableName());
        assertEquals(List.of("column1", "column2", "column3"), insertStatement.getColumns());
        assertEquals(List.of(List.of("1", "2", "3"), List.of("4", "5", "6"), List.of("7", "8", "9")),
            insertStatement.getValues());

    }

}