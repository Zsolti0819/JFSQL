package com.github.jfsql.driver.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import java.io.IOException;
import java.nio.file.Files;
import java.sql.SQLException;
import java.util.Collections;
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
    static void beforeAll() throws SQLException, IOException {
        Files.createDirectories(TestUtils.DATABASE_JSON_FILE_PATH.getParent());
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
            new Entry(entry1ColumnsAndTypes),
            new Entry(entry2ColumnsAndTypes),
            new Entry(entry3ColumnsAndTypes),
            new Entry(entry4ColumnsAndTypes)
        );
        final Schema schema = new Schema(String.valueOf(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH), returnColumnsAndTypes,
            notNullColumns);
        table = new Table("myTable", String.valueOf(TestUtils.TABLE_JSON_FILE_PATH), schema, returnEntries);
        database = new Database(TestUtils.DATABASE_JSON_FILE_PATH, List.of(table));
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
        final Schema schema = reader.readSchema(String.valueOf(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH));
        assertEquals(table.getSchema(), schema);
    }

    @Test
    void testReader_readTablesFromDatabaseFile() throws SQLException {
        final List<Table> tables = reader.readTablesFromDatabaseFile(database);
        // Because we don't read the table's entries at this point
        final Schema schema = new Schema(table.getSchema().getSchemaFile(), table.getSchema().getColumnsAndTypes(),
            table.getSchema().getNotNullColumns());
        final Table tableWithoutEntries = new Table(table.getName(), table.getTableFile(), schema,
            Collections.emptyList());
        final List<Table> returnTables = List.of(tableWithoutEntries);
        assertEquals(returnTables, tables);
    }
}
