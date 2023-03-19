package com.github.jfsql.driver.jdbc.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class InsertExceptionsTest {

    private Statement statement;

    @AfterEach
    void tearDown() throws SQLException {
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
    }

    private void setUp(final String format) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", format);
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testInsert_notValidValue(final String format) throws SQLException {
        setUp(format);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES ('a', 'Zsolti', 25)"));
        assertEquals("Some value's type didn't match the type of the column, to which it was intended to be inserted.",
            thrown.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testInsert_columnNotExists(final String format) throws SQLException {
        setUp(format);

        final SQLException thrown = assertThrows(SQLException.class, () -> statement.executeUpdate(
            "INSERT INTO myTable (lol, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals("Some columns entered doesn't exist in \"myTable\".", thrown.getMessage());

    }

}
