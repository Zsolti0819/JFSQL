package com.github.jfsql.driver.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

class ReaderXmlImplTest {

    private static final Reader reader = new ReaderXmlImpl();
    private static Table table;
    private static Database database;

    @BeforeAll
    static void beforeAll() throws IOException {
        table = Table.builder()
            .name("myTable")
            .tableFile(String.valueOf(TestUtils.TEST_XML_TABLE_PATH))
            .schemaFile(String.valueOf(TestUtils.TEST_XSD_PATH))
            .build();
        database = new Database(TestUtils.TEST_XML_DATABASE_FILE_PATH, List.of(table));
        // Since the database file contains absolute paths, we will create it instead of manipulating with a static file
        new WriterXmlImpl(true).writeDatabaseFile(database);
    }

    @AfterAll
    static void deleteDatabaseFolder() throws IOException {
        Files.delete(TestUtils.TEST_XML_DATABASE_FILE_PATH);
    }

    @Order(1)
    @Test
    void testReader_setSchema() throws IOException {
        final Map<String, String> expectedColumnsAndTypes = new LinkedHashMap<>();
        expectedColumnsAndTypes.put("id", "INTEGER");
        expectedColumnsAndTypes.put("name", "TEXT");
        expectedColumnsAndTypes.put("age", "INTEGER");
        final Map<String, Boolean> expectedNotNullColumns = new LinkedHashMap<>();
        expectedNotNullColumns.put("id", true);
        expectedNotNullColumns.put("name", true);
        expectedNotNullColumns.put("age", true);

        reader.setTableMetaDataFromSchema(table);

        assertEquals(expectedColumnsAndTypes, table.getColumnsAndTypes());
        assertEquals(expectedNotNullColumns, table.getNotNullColumns());
    }

    @Order(2)
    @Test
    void testReader_readEntriesFromTable() throws IOException {
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
        final List<Entry> expectedEntries = List.of(
            new Entry(entry1ColumnsAndTypes, new HashMap<>()),
            new Entry(entry2ColumnsAndTypes, new HashMap<>()),
            new Entry(entry3ColumnsAndTypes, new HashMap<>()),
            new Entry(entry4ColumnsAndTypes, new HashMap<>())
        );

        final List<Entry> entries = reader.readEntriesFromTable(table);

        assertEquals(expectedEntries, entries);
    }

    @Order(3)
    @Test
    void testReader_readTablesFromDatabaseFile() throws IOException {
        final List<Table> tables = reader.readTablesFromDatabaseFile(database);
        // Because we don't read the table's entries at this point
        final Table tableWithoutEntries = Table.builder()
            .name(table.getName())
            .tableFile(table.getTableFile())
            .schemaFile(table.getSchemaFile())
            .entries(Collections.emptyList())
            .build();

        reader.setTableMetaDataFromSchema(tableWithoutEntries);

        final List<Table> returnTables = List.of(tableWithoutEntries);

        assertEquals(returnTables, tables);
    }
}
