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
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class ReaderXmlImplTest {

    private static final Reader reader = new ReaderXmlImpl();
    private static Table table;
    private static Database database;
    private static List<Entry> returnEntries;

    @BeforeAll
    static void beforeAll() throws SQLException {
        database = new Database(TestUtils.DATABASE_XML_FILE_PATH, new LinkedList<>());
        try (final Git ignored = Git.init().setDirectory(database.getUrl().getParent().toFile()).call()) {
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
            returnEntries = List.of(
                new Entry(entry1ColumnsAndTypes),
                new Entry(entry2ColumnsAndTypes),
                new Entry(entry3ColumnsAndTypes),
                new Entry(entry4ColumnsAndTypes)
            );
            table = new Table("myTable", String.valueOf(TestUtils.TABLE_XML_FILE_PATH),
                String.valueOf(TestUtils.TABLE_XSD_FILE_PATH), returnColumnsAndTypes, notNullColumns, returnEntries);
            database.setTables(List.of(table));
            Files.createFile(TestUtils.TABLE_XML_FILE_PATH);
            Files.createFile(TestUtils.TABLE_XSD_FILE_PATH);
        } catch (final IOException | GitAPIException e) {
            e.printStackTrace();
        }
        new WriterXmlImpl(true).writeSchema(table);
        new WriterXmlImpl(true).writeTable(table);
        new WriterXmlImpl(true).writeDatabaseFile(database);
    }

    @AfterAll
    static void deleteDatabaseFolder() {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testReader_readTable() throws SQLException {
        final List<Entry> entries = reader.readTable(table);
        assertEquals(returnEntries, entries);
    }

    @Test
    void testReader_readSchemaForTable() throws SQLException {
        final Table table = reader.readSchema(String.valueOf(TestUtils.TABLE_XSD_FILE_PATH));
        final Table tableWithoutEntries = new Table(table.getName(), table.getTableFile(), table.getSchemaFile(),
            table.getColumnsAndTypes(), table.getNotNullColumns(), Collections.emptyList());
        assertEquals(tableWithoutEntries, table);
    }

    @Test
    void testReader_readDatabaseFile() throws SQLException {
        final List<Table> tables = reader.readDatabaseFile(database);
        // Because we don't read the table's entries at this point
        final Table tableWithoutEntries = new Table(table.getName(), table.getTableFile(), table.getSchemaFile(),
            table.getColumnsAndTypes(), table.getNotNullColumns(), Collections.emptyList());
        final List<Table> returnTables = List.of(tableWithoutEntries);
        assertEquals(returnTables, tables);
    }
}
