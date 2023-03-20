package com.github.jfsql.driver.jdbc.json;

import static org.junit.jupiter.api.Assertions.assertEquals;

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

class CreateTableJsonTest {

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
    void testCreateTable_normally() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate(
            "CREATE TABLE myTable (id INTEGER NOT NULL, name TEXT NOT NULL, age INTEGER NOT NULL)"));
        final String realTableFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContent = "" +
            "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedTableFileContent, realTableFileContent);
        final String realSchemaFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedSchemaFileContent = "" +
            "{\n" +
            "  \"$schema\": \"http://json-schema.org/draft-06/schema#\",\n" +
            "  \"type\": \"object\",\n" +
            "  \"required\": [\n" +
            "    \"Entry\"\n" +
            "  ],\n" +
            "  \"properties\": {\n" +
            "    \"Entry\": {\n" +
            "      \"type\": \"array\",\n" +
            "      \"items\": {\n" +
            "        \"type\": \"object\",\n" +
            "        \"required\": [\n" +
            "          \"id\",\n" +
            "          \"name\",\n" +
            "          \"age\"\n" +
            "        ],\n" +
            "        \"properties\": {\n" +
            "          \"id\": {\n" +
            "            \"type\": [\n" +
            "              \"integer\"\n" +
            "            ]\n" +
            "          },\n" +
            "          \"name\": {\n" +
            "            \"type\": [\n" +
            "              \"string\"\n" +
            "            ]\n" +
            "          },\n" +
            "          \"age\": {\n" +
            "            \"type\": [\n" +
            "              \"integer\"\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      }\n" +
            "    }\n" +
            "  }\n" +
            "}";
        assertEquals(expectedSchemaFileContent, realSchemaFileContent);
    }

}
