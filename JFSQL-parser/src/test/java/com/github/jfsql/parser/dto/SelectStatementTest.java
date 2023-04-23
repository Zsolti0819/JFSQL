package com.github.jfsql.parser.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.parser.core.Parser;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SelectStatementTest {

    private Parser parser;

    @BeforeEach
    void setUp() {
        parser = new Parser();
    }

    @Test
    void testSelectStatement() {
        final String statement = "SELECT customerName, customerCity, customerMail, salesTotal FROM onlineCustomers INNER JOIN orders ON onlineCustomers.customerId = orders.customerId INNER JOIN sales ON orders.orderId = sales.orderId WHERE sales_id = 1 LIMIT 1 OFFSET 0";
        final SelectStatement selectStatement = (SelectStatement) parser.parse(statement);
        assertEquals("onlineCustomers", selectStatement.getTableName());
        assertEquals(List.of("orders", "sales"), selectStatement.getJoinTableNames());
        assertEquals(List.of(JoinType.INNER_JOIN, JoinType.INNER_JOIN), selectStatement.getJoinTypes());
        assertEquals(List.of("customerName", "customerCity", "customerMail", "salesTotal"),
            selectStatement.getColumns());
        assertEquals(List.of(List.of("onlineCustomers.customerId", "orders.customerId"),
            List.of("orders.orderId", "sales.orderId")), selectStatement.getListOfJoinColumns());
        assertEquals(List.of("sales_id"), selectStatement.getWhereColumns());
        assertEquals(List.of("1"), selectStatement.getWhereValues());
        assertEquals(List.of("="), selectStatement.getSymbols());
        assertEquals(List.of(), selectStatement.getBinaryOperators());
        assertEquals("1", selectStatement.getLimit());
        assertEquals("0", selectStatement.getOffset());

    }

}