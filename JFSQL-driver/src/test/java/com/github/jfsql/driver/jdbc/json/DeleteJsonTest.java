package com.github.jfsql.driver.jdbc.json;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class DeleteJsonTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "json");
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (2, 'Tomi', 24)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (3, 'Ivan', 26)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (4, 'Lukas', 34)");
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testDelete_columnsNotExist() {
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.execute("DELETE FROM myTable WHERE lol > 3 AND age > 25 AND name = 'Lukas'"));
        assertEquals("Some columns entered doesn't exist in \"myTable\".", thrown.getMessage());

    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas'"
    })
    void testDelete_multipleANDs(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(1, statement.executeUpdate(sql));
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
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE name = 'Zsolti' OR age = 24 OR id = 3 OR name = 'Lukas'"
    })
    void testDelete_multipleORs(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(4, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE id = 1 AND name = 'Zsolti' and age = 25"
    })
    void testDelete_multipleANDsSameEntry(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(1, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
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

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas' OR name = 'Zsolti'"
    })
    void testDelete_multipleBinaryOperators(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(2, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE name = 'Tomi'"
    })
    void testDelete_equals(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(1, statement.executeUpdate(sql));
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

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE age <= 34"
    })
    void testDelete_lte(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(4, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable"
    })
    void testDelete_withoutWhere(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertEquals(4, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

}
