package com.github.jfsql.driver.core;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class JfsqlDriverTest {

    private static final String URL = "jdbc:jfsql:" + TestUtils.DATABASE_PATH;
    private static Driver driver;

    @BeforeAll
    static void setUp() {
        driver = new JfsqlDriver();
    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void acceptsURL() throws SQLException {
        assertTrue(driver.acceptsURL(URL));
        assertFalse(driver.acceptsURL(null));
        assertFalse(driver.acceptsURL("invalid url"));
    }

    @Test
    void connect() throws SQLException {
        final Properties info = new Properties();
        final Connection connection = driver.connect(URL, info);
        assertNotNull(connection);
        connection.close();
    }

    @Test
    void connectThrowsException() {
        final Properties info = new Properties();
        final String invalidUrl = "jdbc:invalid:url";
        assertThrows(SQLException.class, () -> {
            try (final Connection connection = driver.connect(invalidUrl, info)) {
                connection.createStatement();
            }
        });
    }

    @Test
    void getMajorVersion() {
        assertEquals(1, driver.getMajorVersion());
    }

    @Test
    void getMinorVersion() {
        assertEquals(0, driver.getMinorVersion());
    }

    @Test
    void jdbcCompliant() {
        assertFalse(driver.jdbcCompliant());
    }

    @Test
    void getPropertyInfo() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> driver.getPropertyInfo(null, null));
    }

    @Test
    void getParentLogger() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> driver.getParentLogger());
    }
}
