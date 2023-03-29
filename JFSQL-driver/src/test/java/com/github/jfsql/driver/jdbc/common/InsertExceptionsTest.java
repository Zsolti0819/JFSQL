package com.github.jfsql.driver.jdbc.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class InsertExceptionsTest {

    private Statement statement;
    private Connection connection;

    private void setUp(final String format) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", format);
        properties.setProperty("transaction.versioning", "true");
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
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
    void testInsert_notValidValue(final String format) throws SQLException {
        setUp(format);

        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES ('a', 'Zsolti', 25)"));
        assertEquals("Some value's type didn't match the type of the column, to which it was intended to be inserted.",
            thrown.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testInsert_columnNotExists(final String format) throws SQLException {
        setUp(format);

        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        final SQLException thrown = assertThrows(SQLException.class, () -> statement.executeUpdate(
            "INSERT INTO myTable (lol, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals("Some columns entered doesn't exist in 'myTable'.", thrown.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testInsert_preparedStatement_insertNullIntoNotNullColumn(final String format) throws SQLException {
        setUp(format);

        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB NOT NULL)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        final SQLException thrown = assertThrows(SQLException.class, preparedStatement::executeUpdate);
        assertEquals("Inserting null value into a NOT NULL column.", thrown.getMessage());
    }
}
