package com.github.jfsql.driver.jdbc.json;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlResultSet;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class InsertJsonTest {

    private Connection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "json");
        connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testInsert_simple() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testInsert_preparedStatement_simple() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": null\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testInsert_preparedStatement_blob() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_FILE_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": \"" + String.valueOf(TestUtils.ENCODED_BLOB_PATH_JSON).replace("\\", "\\\\") + "\"\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_FILE_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_FILE_PATH.toFile().exists());
    }

    @Test
    void testInsert_multiRow() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\",\n" +
            "      \"age\": 34\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testInsert_noExplicitColumns() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\",\n" +
            "      \"age\": 34\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testInsert_notValidValue() {
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES ('a', 'Zsolti', 25)"));
        assertEquals("Some value's type didn't match the type of the column, to which it was intended to be inserted.",
            thrown.getMessage());
    }

    @Test
    void testInsert_columnNotExists() {
        final SQLException thrown = assertThrows(SQLException.class, () -> statement.executeUpdate(
            "INSERT INTO myTable (lol, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals("Some columns entered doesn't exist in \"myTable\".", thrown.getMessage());

    }

    @Test
    void testInsert_increasesResultSetSize() throws SQLException {
        final JfsqlResultSet resultSetBefore = (JfsqlResultSet) statement.executeQuery("SELECT * FROM myTable");
        assertEquals(0, resultSetBefore.getEntries().size());
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        final JfsqlResultSet resultSetAfter = (JfsqlResultSet) statement.executeQuery("SELECT * FROM myTable");
        assertEquals(4, resultSetAfter.getEntries().size());
    }

    @Test
    void testInsert_multiple() throws SQLException {
        assertEquals(2,
            statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24)"));
        assertEquals(3, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24)"));
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals(2, statement.executeUpdate("INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24)"));
        assertEquals(3,
            statement.executeUpdate("INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24)"));
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
    }

}
