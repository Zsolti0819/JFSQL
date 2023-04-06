package com.github.jfsql.driver.jdbc;

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
import org.junit.jupiter.params.provider.MethodSource;

class CreateDatabaseExceptionsTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
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
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateDatabase_databaseIsNotDirectory(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE DATABASE [" + TestUtils.NOT_DIRECTORY_PATH + "];"));
        assertEquals("Database is not a directory.", thrown.getMessage());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateDatabase_databaseExists(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertEquals("Database already exists.", thrown.getMessage());
    }

}
