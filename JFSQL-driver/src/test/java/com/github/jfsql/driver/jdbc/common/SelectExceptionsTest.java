package com.github.jfsql.driver.jdbc.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class SelectExceptionsTest {

    private Statement statement;
    private Connection connection;

    static Stream<Arguments> configurations() {
        return Stream.of(
            Arguments.of("json", "jgit"),
            Arguments.of("json", "none"),
            Arguments.of("xml", "jgit"),
            Arguments.of("xml", "none")
        );
    }

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
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
    @MethodSource("configurations")
    void testSelect_columnsNotInResultSet(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("DROP TABLE IF EXISTS myTable2");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");
        final ResultSet resultSet = statement.executeQuery("SELECT name FROM myTable WHERE id=3");
        while (resultSet.next()) {
            assertEquals("Ivan", resultSet.getString("name"));
            final SQLException thrown = assertThrows(SQLException.class, () -> resultSet.getInt("id"));
            assertEquals("Column 'id' doesn't exist in the ResultSet.", thrown.getMessage());
        }
    }

    @ParameterizedTest
    @MethodSource("configurations")
    void testSelect_preparedStatement_columnsNotInResultSet(final String persistence,
        final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("DROP TABLE IF EXISTS myTable2");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT name FROM myTable WHERE id = ?");
        preparedStatement.setInt(1, 3);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            assertEquals("Ivan", resultSet.getString("name"));
            final SQLException thrown = assertThrows(SQLException.class, () -> resultSet.getInt("id"));
            assertEquals("Column 'id' doesn't exist in the ResultSet.", thrown.getMessage());
        }
    }

}
