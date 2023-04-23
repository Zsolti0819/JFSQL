package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class UpdateStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @Test
    void testUpdateStatement() {
        final String statement = "UPDATE myTable SET name = 'TomiEdited' WHERE age <= 24";
        final UpdateStatement updateStatement = (UpdateStatement) parser.parse(statement);
        assertEquals("myTable", updateStatement.getTableName());
        assertEquals(List.of("name"), updateStatement.getColumns());
        assertEquals(List.of("TomiEdited"), updateStatement.getValues());
        assertEquals(List.of("age"), updateStatement.getWhereColumns());
        assertEquals(List.of("24"), updateStatement.getWhereValues());
        assertEquals(List.of("<="), updateStatement.getSymbols());

    }

}