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

class CreateDatabaseExceptionsTest {

    private Statement statement;

    private void setUp(final String format) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", format);
        properties.setProperty("transaction.versioning", "true");
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
    }

    @AfterEach
    void tearDown() {
        try {
            statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
        } catch (final SQLException e) {
            TestUtils.deleteDatabaseDirectory();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testCreateDatabase_databaseIsNotDirectory(final String format) throws SQLException {
        setUp(format);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE DATABASE [" + TestUtils.NOT_DIRECTORY_PATH + "];"));
        assertEquals("Database is not a directory.", thrown.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testCreateDatabase_databaseExists(final String format) throws SQLException {
        setUp(format);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertEquals("Database already exists, will not create another one.", thrown.getMessage());
    }

}
