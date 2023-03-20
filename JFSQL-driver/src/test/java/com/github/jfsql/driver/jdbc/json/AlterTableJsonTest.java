package com.github.jfsql.driver.jdbc.json;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class AlterTableJsonTest {

    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "json");
        properties.setProperty("transaction.versioning", "true");
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (2, 'Tomi', 24)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (3, 'Ivan', 26)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (4, 'Lukas', 34)");
    }

    @AfterEach
    void tearDown() {
        try {
            statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
        } catch (final SQLException e) {
            TestUtils.deleteDatabaseDirectory();
        }
    }

    @Test
    void testAlterTable_renameTable() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable RENAME to myTableEdited;");
        final String realDatabaseFileContentAfter = FileUtils.readFileToString(
            TestUtils.DATABASE_JSON_FILE_PATH.toFile(), StandardCharsets.UTF_8);
        assertTrue(realDatabaseFileContentAfter.contains("myTableEdited.json"));
        assertTrue(realDatabaseFileContentAfter.contains("myTableEditedSchema.json"));
        assertFalse(realDatabaseFileContentAfter.contains("myTable.json"));
        assertFalse(realDatabaseFileContentAfter.contains("myTableSchema.json"));
        final String realTableFileContentAfter = FileUtils.readFileToString(
            TestUtils.EDITED_TABLE_JSON_FILE_PATH.toFile(), StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
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
        assertEquals(expectedTableFileContentAfter, realTableFileContentAfter);
        assertFalse(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.EDITED_TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());
    }

    @Test
    void testAlterTable_renameColumn() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable RENAME COLUMN age TO age_edited;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age_edited\": 25\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age_edited\": 24\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age_edited\": 26\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\",\n" +
            "      \"age_edited\": 34\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedTableFileContentAfter, realTableFileContentAfter);
    }

    @Test
    void testAlterTable_dropColumn() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable DROP COLUMN age;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\"\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\"\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\"\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\"\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedTableFileContentAfter, realTableFileContentAfter);
    }

    @Test
    void testAlterTable_addColumn() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"salary\": null\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24,\n" +
            "      \"salary\": null\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26,\n" +
            "      \"salary\": null\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\",\n" +
            "      \"age\": 34,\n" +
            "      \"salary\": null\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedTableFileContentAfter, realTableFileContentAfter);
    }

    @Test
    void testAlterTable_addNotNullColumn() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL NOT NULL;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"salary\": 0.0\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24,\n" +
            "      \"salary\": 0.0\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26,\n" +
            "      \"salary\": 0.0\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\",\n" +
            "      \"age\": 34,\n" +
            "      \"salary\": 0.0\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedTableFileContentAfter, realTableFileContentAfter);
    }

}
