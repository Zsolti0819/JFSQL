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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DropDatabaseJsonTest {

    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "json");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
    }

    @Test
    void testDropDatabase() throws SQLException {
        assertTrue(TestUtils.DATABASE_JSON_FILE_PATH.toFile().exists());
        assertEquals(1, statement.executeUpdate("DROP DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertFalse(TestUtils.DATABASE_JSON_FILE_PATH.toFile().exists());
    }

}
