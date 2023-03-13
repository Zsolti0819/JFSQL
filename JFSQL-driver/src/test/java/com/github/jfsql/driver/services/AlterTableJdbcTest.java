package com.github.jfsql.driver.services;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class AlterTableJdbcTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH);
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
    void testAlterTable_renameTable_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
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
    void testAlterTable_renameTable_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        statement.execute("ALTER TABLE myTable RENAME to myTableEdited;");
        final String realDatabaseFileContentAfter = FileUtils.readFileToString(
                TestUtils.DATABASE_XML_FILE_PATH.toFile(), StandardCharsets.UTF_8);
        assertTrue(realDatabaseFileContentAfter.contains("myTableEdited.xml"));
        assertTrue(realDatabaseFileContentAfter.contains("myTableEdited.xsd"));
        assertFalse(realDatabaseFileContentAfter.contains("myTable.xml"));
        assertFalse(realDatabaseFileContentAfter.contains("myTable.xsd"));
        final String realTableFileContentAfter = FileUtils.readFileToString(
                TestUtils.EDITED_TABLE_XML_FILE_PATH.toFile(), StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
                "<myTableEdited>\n" +
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
                "</myTableEdited>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContentAfter),
                StringUtils.deleteWhitespace(realTableFileContentAfter));
        assertFalse(TestUtils.TABLE_XSD_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.EDITED_TABLE_XSD_FILE_PATH.toFile().exists());
    }

    @Test
    void testAlterTable_renameColumn_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
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
    void testAlterTable_renameColumn_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        statement.execute("ALTER TABLE myTable RENAME COLUMN age TO age_edited;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
                "<myTable>\n" +
                "    <Entry>\n" +
                "        <id>1</id>\n" +
                "        <name>Zsolti</name>\n" +
                "        <age_edited>25</age_edited>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>2</id>\n" +
                "        <name>Tomi</name>\n" +
                "        <age_edited>24</age_edited>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>3</id>\n" +
                "        <name>Ivan</name>\n" +
                "        <age_edited>26</age_edited>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>4</id>\n" +
                "        <name>Lukas</name>\n" +
                "        <age_edited>34</age_edited>\n" +
                "    </Entry>\n" +
                "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContentAfter),
                StringUtils.deleteWhitespace(realTableFileContentAfter));
    }

    @Test
    void testAlterTable_dropColumn_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
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
    void testAlterTable_dropColumn_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        statement.execute("ALTER TABLE myTable DROP COLUMN age;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
                "<myTable>\n" +
                "    <Entry>\n" +
                "        <id>1</id>\n" +
                "        <name>Zsolti</name>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>2</id>\n" +
                "        <name>Tomi</name>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>3</id>\n" +
                "        <name>Ivan</name>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>4</id>\n" +
                "        <name>Lukas</name>\n" +
                "    </Entry>\n" +
                "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContentAfter),
                StringUtils.deleteWhitespace(realTableFileContentAfter));
    }

    @Test
    void testAlterTable_addColumn_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
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
    void testAlterTable_addColumn_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = "" +
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
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContentAfter),
                StringUtils.deleteWhitespace(realTableFileContentAfter));
    }

}
