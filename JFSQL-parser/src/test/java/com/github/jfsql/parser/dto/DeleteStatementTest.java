package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DeleteStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @Test
    void testAlterTableStatement() {
        final String statement = "DELETE FROM myTable WHERE lol > 3 AND age > 25 AND name = 'Lukas'";
        final DeleteStatement deleteStatement = (DeleteStatement) parser.parse(statement);
        assertEquals("myTable", deleteStatement.getTableName());
        assertEquals(List.of("lol", "age", "name"), deleteStatement.getWhereColumns());
        assertEquals(List.of("3", "25", "Lukas"), deleteStatement.getWhereValues());
        assertEquals(List.of(">", ">", "="), deleteStatement.getSymbols());
        assertEquals(List.of("AND", "AND"), deleteStatement.getBinaryOperators());
    }

    @Test
    void testAlterTableStatement_withoutWhere() {
        final String statement = "DELETE FROM myTable;";
        final DeleteStatement deleteStatement = (DeleteStatement) parser.parse(statement);
        assertEquals("myTable", deleteStatement.getTableName());
        assertEquals(Collections.emptyList(), deleteStatement.getWhereColumns());
        assertEquals(Collections.emptyList(), deleteStatement.getWhereValues());
        assertEquals(Collections.emptyList(), deleteStatement.getSymbols());
        assertEquals(Collections.emptyList(), deleteStatement.getBinaryOperators());
    }

}