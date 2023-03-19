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

class DeleteExceptionsTest {

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
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testDelete_columnsNotExist(final String format) throws SQLException {
        setUp(format);

        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)");
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.execute("DELETE FROM myTable WHERE lol > 3 AND age > 25 AND name = 'Lukas'"));
        assertEquals("Some columns entered doesn't exist in \"myTable\".", thrown.getMessage());
    }

}
