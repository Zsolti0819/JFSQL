package com.github.jfsql.driver.jdbc.json;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DropTableJsonTest {

    private static Statement statement;

    @AfterAll
    static void afterAll() throws SQLException {
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
    }

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "json");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @Test
    void testDropTable_normally() throws SQLException {
        assertTrue(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());
        assertFalse(statement.execute("DROP TABLE myTable"));
        assertFalse(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertFalse(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());
    }

    @Test
    void testDropTable_correctEntryCount() throws SQLException {
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)");
        assertEquals(4, statement.executeUpdate("DROP TABLE myTable"));
    }
}
