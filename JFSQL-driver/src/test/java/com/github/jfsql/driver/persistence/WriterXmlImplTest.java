package com.github.jfsql.driver.persistence;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class WriterXmlImplTest {

    private static final Writer writer = new WriterXmlImpl();
    private static Table table;
    private static Database database;

    @BeforeAll
    static void setUp() {
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
            table = new Table("myTable", String.valueOf(TestUtils.TABLE_XML_FILE_PATH),
                String.valueOf(TestUtils.TABLE_XSD_FILE_PATH), returnColumnsAndTypes, notNullColumns);
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
            table.setEntries(returnEntries);
            database = new Database(TestUtils.DATABASE_XML_FILE_PATH, new LinkedList<>());
            database.setTables(List.of(table));
        } catch (final GitAPIException e) {
            e.printStackTrace();
        }
    }

    @AfterAll
    static void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testWriter_writeSchema() throws SQLException, IOException {
        writer.writeSchema(table);
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XSD_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">\n" +
            "    <xs:element name=\"myTable\">\n" +
            "        <xs:complexType>\n" +
            "            <xs:sequence>\n" +
            "                <xs:element maxOccurs=\"unbounded\" minOccurs=\"0\" name=\"Entry\">\n" +
            "                    <xs:complexType>\n" +
            "                        <xs:sequence>\n" +
            "                            <xs:element name=\"id\" type=\"xs:long\"/>\n" +
            "                            <xs:element name=\"name\" type=\"xs:string\"/>\n" +
            "                            <xs:element name=\"age\" type=\"xs:long\"/>\n" +
            "                        </xs:sequence>\n" +
            "                    </xs:complexType>\n" +
            "                </xs:element>\n" +
            "            </xs:sequence>\n" +
            "        </xs:complexType>\n" +
            "    </xs:element>\n" +
            "</xs:schema>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));

    }

    @Test
    void testWriter_writeTable() throws SQLException, IOException {
        writer.writeTable(table);
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>2</id>\n" +
            "        <name>Tomi</name>\n" +
            "        <age>24</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>3</id>\n" +
            "        <name>Ivan</name>\n" +
            "        <age>26</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testWriter_writeDatabaseFile() {
        assertDoesNotThrow(() -> writer.writeDatabaseFile(database));
    }
}