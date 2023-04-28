package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
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

class CreateTableExceptionsTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection(TestUtils.URL, properties);
        statement = connection.createStatement();
    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateTable_sameTableNameAndDatabaseName(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE TABLE myDatabase (id INTEGER, name TEXT, age INTEGER)"));
        assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateTable_tableExists(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)"));
        assertEquals("Table 'myTable' already exists.", thrown.getMessage());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateTable_ifNotExists_doesNotThrowException(final String persistence,
        final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        assertDoesNotThrow(
            () -> statement.executeUpdate("CREATE TABLE IF NOT EXISTS myTable (id INTEGER, name TEXT, age INTEGER)"));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateTable_duplicateColumns(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE TABLE myTable (name TEXT, name TEXT, age INTEGER)"));
        assertEquals("Duplicate columns were found in the statement.", thrown.getMessage());
    }

}
