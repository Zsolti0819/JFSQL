package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.core.Parser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class InsertStatementTest {

    Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @ParameterizedTest
    @CsvSource({
            "'INSERT INTO myTable (column1, column2, column3) VALUES (1, 2, 3), (4, 5, 6), (7, 8, 9);','myTable','column1','column2','column3','1','2','3','4','5','6','7','8','9'",
    })
    void testInsertStatements(final String query, final String tableName, final String column1Name, final String column2Name, final String column3Name, final String column1Value1, final String column1Value2, final String column1Value3, final String column2Value1, final String column2Value2, final String column2Value3, final String column3Value1, final String column3Value2, final String column3Value3) {
        final InsertStatement insertStatement = (InsertStatement) parser.parse(query);
        assertEquals(tableName, insertStatement.getTableName());
        assertEquals(column1Name, insertStatement.getColumns().get(0));
        assertEquals(column2Name, insertStatement.getColumns().get(1));
        assertEquals(column3Name, insertStatement.getColumns().get(2));
        assertEquals(column1Value1, insertStatement.getValues().get(0).get(0));
        assertEquals(column1Value2, insertStatement.getValues().get(0).get(1));
        assertEquals(column1Value3, insertStatement.getValues().get(0).get(2));
        assertEquals(column2Value1, insertStatement.getValues().get(1).get(0));
        assertEquals(column2Value2, insertStatement.getValues().get(1).get(1));
        assertEquals(column2Value3, insertStatement.getValues().get(1).get(2));
        assertEquals(column3Value1, insertStatement.getValues().get(2).get(0));
        assertEquals(column3Value2, insertStatement.getValues().get(2).get(1));
        assertEquals(column3Value3, insertStatement.getValues().get(2).get(2));
    }
}