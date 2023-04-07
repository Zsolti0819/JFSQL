package com.github.jfsql.driver.core;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class JfsqlDriverTest {

    private static final String URL = "jdbc:jfsql:database";
    private static Driver driver;

    @BeforeAll
    static void setUp() {
        driver = new JfsqlDriver();
    }

    @AfterAll
    static void tearDown() throws SQLException {
        DriverManager.deregisterDriver(driver);
    }

    @Test
    void testAcceptsURL() throws SQLException {
        assertTrue(driver.acceptsURL(URL));
        assertFalse(driver.acceptsURL(null));
        assertFalse(driver.acceptsURL("invalid url"));
    }

    @Test
    void testConnect() throws SQLException {
        final Properties info = new Properties();
        final Connection connection = driver.connect(URL, info);
        assertNotNull(connection);
        connection.close();
    }

    @Test
    void testConnectThrowsException() {
        final Properties info = new Properties();
        final String invalidUrl = "jdbc:invalid:url";
        assertThrows(SQLException.class, () -> {
            try (final Connection connection = driver.connect(invalidUrl, info)) {
                connection.createStatement();
            }
        });
    }
}
