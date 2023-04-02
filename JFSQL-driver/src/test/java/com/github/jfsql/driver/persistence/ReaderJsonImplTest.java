package com.github.jfsql.driver.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import java.io.IOException;
import java.nio.file.Files;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class ReaderJsonImplTest {

    private static final Reader reader = new ReaderJsonImpl();
    private static Table table;
    private static Database database;

    @BeforeAll
    static void beforeAll() throws IOException {
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
        new WriterJsonImpl(true).writeSchema(table);
        new WriterJsonImpl(true).writeTable(table);
        new WriterJsonImpl(true).writeDatabaseFile(database);
    }

    @AfterAll
    static void deleteDatabaseFolder() {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testReader_readEntriesFromTable() throws SQLException {
        final List<Entry> entries = reader.readEntriesFromTable(table);
        assertEquals(table.getEntries(), entries);
    }

    @Test
    void testReader_readSchema() throws SQLException {
        final Table schema = reader.readSchema(String.valueOf(TestUtils.JSON_SCHEMA_PATH));
        assertEquals(table.getColumnsAndTypes(), schema.getColumnsAndTypes());
        assertEquals(table.getNotNullColumns(), schema.getNotNullColumns());
        assertEquals(table.getSchemaFile(), schema.getSchemaFile());
    }

    @Test
    void testReader_readTablesFromDatabaseFile() throws SQLException {
        final List<Table> tables = reader.readTablesFromDatabaseFile(database);
        // Because we don't read the table's entries at this point
        final Table schema = Table.builder()
            .schemaFile(table.getSchemaFile())
            .columnsAndTypes(table.getColumnsAndTypes())
            .notNullColumns(table.getNotNullColumns())
            .build();
        final Table tableWithoutEntries = Table.builder()
            .name(table.getName())
            .tableFile(table.getTableFile())
            .schemaFile(schema.getSchemaFile())
            .columnsAndTypes(schema.getColumnsAndTypes())
            .notNullColumns(schema.getNotNullColumns())
            .entries(Collections.emptyList())
            .build();
        final List<Table> returnTables = List.of(tableWithoutEntries);
        assertEquals(returnTables, tables);
    }
}
