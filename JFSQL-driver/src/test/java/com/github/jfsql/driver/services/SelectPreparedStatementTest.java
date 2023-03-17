package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import java.io.IOException;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SelectPreparedStatementTest {

    private JfsqlConnection connection;

    @BeforeEach
    void setUp() throws SQLException {
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH);
        final Statement statement = connection.createStatement();
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");
        statement.execute("CREATE TABLE myTable2 (id INTEGER , name2 TEXT, age2 INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable2 (id, name2, age2) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 1)");

    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testSelect_preparedStatement_whereIntegerGt() throws SQLException {
        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE id > 1");
        preparedStatement.setInt(1, 1);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {24, 26, 34};
        final String[] expectedNamesArray = {"Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {2, 3, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @Test
    void testSelect_preparedStatement_whereLike() throws SQLException {
        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE name LIKE ? OR name LIKE ?");
        preparedStatement.setString(1, "Z_olti");
        preparedStatement.setString(2, "%uka%");
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {25, 34};
        final String[] expectedNamesArray = {"Zsolti", "Lukas"};
        final Integer[] expectedIdsArray = {1, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @Test
    void testSelect_preparedStatement_whereMultipleANDs() throws SQLException {
        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE id > ? AND age >= ? AND name = ?");
        preparedStatement.setInt(1, 1);
        preparedStatement.setInt(2, 24);
        preparedStatement.setString(3, "Lukas");
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {34};
        final String[] expectedNamesArray = {"Lukas"};
        final Integer[] expectedIdsArray = {4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @Test
    void testSelect_preparedStatement_whereIntegerGte() throws SQLException {
        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE id >= ?");
        preparedStatement.setInt(1, 2);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {24, 26, 34};
        final String[] expectedNamesArray = {"Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {2, 3, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @Test
    void testSelect_preparedStatement_columnsByIndex() throws SQLException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT id, age, name FROM myTable WHERE id = ?");
        preparedStatement.setInt(1, 1);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            assertEquals("Zsolti", resultSet.getString(3));
            assertEquals(25, resultSet.getInt(2));
            assertEquals(1, resultSet.getInt(1));
        }
    }

    @Test
    void testSelect_preparedStatement_columnsByColumnName() throws SQLException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT id, age, name FROM myTable WHERE id = ?");
        preparedStatement.setInt(1, 1);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            assertEquals(25, resultSet.getInt("age"));
            assertEquals("Zsolti", resultSet.getString("name"));
            assertEquals(1, resultSet.getInt("id"));
        }
    }

    @Test
    void testSelect_preparedStatement_columnsNotInResultSet() throws SQLException {
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
