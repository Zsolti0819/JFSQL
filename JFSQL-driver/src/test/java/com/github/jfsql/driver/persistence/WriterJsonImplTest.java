package com.github.jfsql.driver.persistence;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class WriterJsonImplTest {

    private static final Writer writer = new WriterJsonImpl(true);
    private static Table table;
    private static Database database;

    @BeforeAll
    static void setUp() throws IOException {
        Files.createDirectories(TestUtils.JSON_DATABASE_PATH.getParent());
        final Map<String, String> returnColumnsAndTypes = new LinkedHashMap<>();
        returnColumnsAndTypes.put("id", "INTEGER");
        returnColumnsAndTypes.put("name", "TEXT");
        returnColumnsAndTypes.put("age", "INTEGER");
        final Map<String, Boolean> notNullColumns = new LinkedHashMap<>();
        notNullColumns.put("id", true);
        notNullColumns.put("name", true);
        notNullColumns.put("age", true);
        final Map<String, String> entry1ColumnsAndTypes = new LinkedHashMap<>();
        entry1ColumnsAndTypes.put("id", "1");
        entry1ColumnsAndTypes.put("name", "Zsolti");
        entry1ColumnsAndTypes.put("age", "25");
        final Map<String, String> entry2ColumnsAndTypes = new LinkedHashMap<>();
        entry2ColumnsAndTypes.put("id", "2");
        entry2ColumnsAndTypes.put("name", "Tomi");
        entry2ColumnsAndTypes.put("age", "24");
        final Map<String, String> entry3ColumnsAndTypes = new LinkedHashMap<>();
        entry3ColumnsAndTypes.put("id", "3");
        entry3ColumnsAndTypes.put("name", "Ivan");
        entry3ColumnsAndTypes.put("age", "26");
        final Map<String, String> entry4ColumnsAndTypes = new LinkedHashMap<>();
        entry4ColumnsAndTypes.put("id", "4");
        entry4ColumnsAndTypes.put("name", "Lukas");
        entry4ColumnsAndTypes.put("age", "34");
        final List<Entry> returnEntries = List.of(
            new Entry(entry1ColumnsAndTypes, new HashMap<>()),
            new Entry(entry2ColumnsAndTypes, new HashMap<>()),
            new Entry(entry3ColumnsAndTypes, new HashMap<>()),
            new Entry(entry4ColumnsAndTypes, new HashMap<>())
        );
        table = Table.builder()
            .name("myTable")
            .tableFile(String.valueOf(TestUtils.JSON_TABLE_PATH))
            .schemaFile(String.valueOf(TestUtils.JSON_SCHEMA_PATH))
            .columnsAndTypes(returnColumnsAndTypes)
            .notNullColumns(notNullColumns)
            .entries(returnEntries)
            .build();
        database = new Database(TestUtils.JSON_DATABASE_PATH, List.of(table));
    }

    @AfterAll
    static void deleteDatabaseFolder() {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testWriter_writeSchema() throws IOException {
        writer.writeSchema(table);
        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_SCHEMA_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedContent = "{\n" +
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
        assertEquals(expectedContent, realFileContent);
    }

    @Test
    void testWriter_writeTable() throws IOException {
        writer.writeTable(table);
        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedContent = "{\n" +
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
        assertEquals(expectedContent, realFileContent);
    }

    @Test
    void testWriter_writeDatabaseFile() {
        assertDoesNotThrow(() -> writer.writeDatabaseFile(database));
    }
}
