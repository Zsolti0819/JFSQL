package com.github.jfsql.driver.jdbc.common;

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
import org.junit.jupiter.params.provider.ValueSource;

class CreateTableExceptionsTest {

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
    void testCreateTable_sameTableNameAndDatabaseName(final String format) throws SQLException {
        setUp(format);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE TABLE myDatabase (id INTEGER, name TEXT, age INTEGER)"));
        assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testCreateTable_tableExists(final String format) throws SQLException {
        setUp(format);

        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)"));
        assertEquals("Table \"myTable\" already exists.", thrown.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testCreateTable_ifNotExists_doesNotThrowException(final String format) throws SQLException {
        setUp(format);

        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        assertDoesNotThrow(
            () -> statement.executeUpdate("CREATE TABLE IF NOT EXISTS myTable (id INTEGER, name TEXT, age INTEGER)"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testCreateTable_duplicateColumns(final String format) throws SQLException {
        setUp(format);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("CREATE TABLE myTable (name TEXT, name TEXT, age INTEGER)"));
        assertEquals("Some columns were identical during table creation.", thrown.getMessage());
    }

}
