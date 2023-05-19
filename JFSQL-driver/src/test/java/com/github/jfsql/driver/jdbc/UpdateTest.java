package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

public class UpdateTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection(TestUtils.URL, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");

    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testUpdate_oneEntry(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testUpdate_oneEntry_json();
                break;
            case "xml":
                testUpdate_oneEntry_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testUpdate_oneEntry_json() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("UPDATE myTable SET name = 'TomiEdited' WHERE age <= 24"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"TomiEdited\",\n" +
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

    void testUpdate_oneEntry_xml() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("UPDATE myTable SET name = 'TomiEdited' WHERE age <= 24"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>2</id>\n" +
            "        <name>TomiEdited</name>\n" +
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

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testUpdate_oneEntryMultipleCriteria(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testUpdate_oneEntryMultipleCriteria_json();
                break;
            case "xml":
                testUpdate_oneEntryMultipleCriteria_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testUpdate_oneEntryMultipleCriteria_json() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate(
            "UPDATE myTable SET id = 5, name = 'Marian', age=99 WHERE id = 4 AND name = 'Lukas' AND age = 34"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
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
            "      \"id\": 5,\n" +
            "      \"name\": \"Marian\",\n" +
            "      \"age\": 99\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testUpdate_oneEntryMultipleCriteria_xml() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate(
            "UPDATE myTable SET id = 5, name = 'Marian', age=99 WHERE id = 4 AND name = 'Lukas' AND age = 34"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
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
            "        <id>5</id>\n" +
            "        <name>Marian</name>\n" +
            "        <age>99</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testUpdate_noWhereClauseUpdatesAll(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testUpdate_noWhereClauseUpdatesAll_json();
                break;
            case "xml":
                testUpdate_noWhereClauseUpdatesAll_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }


    void testUpdate_noWhereClauseUpdatesAll_json() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate("UPDATE myTable SET name='Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n"
            + "  \"Entry\": [\n"
            + "    {\n"
            + "      \"id\": 1,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 25\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 2,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 24\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 3,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 26\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 4,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 34\n"
            + "    }\n"
            + "  ]\n"
            + "}";
        assertEquals(expectedFileContent, realFileContent);
    }


    void testUpdate_noWhereClauseUpdatesAll_xml() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate("UPDATE myTable SET name='Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
            + "<myTable>\n"
            + "    <Entry>\n"
            + "        <id>1</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>25</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>2</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>24</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>3</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>26</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>4</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>34</age>\n"
            + "    </Entry>\n"
            + "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testUpdate_notExistingEntry(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testUpdate_notExistingEntry_json();
                break;
            case "xml":
                testUpdate_notExistingEntry_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testUpdate_notExistingEntry_json() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE id=5"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n"
            + "  \"Entry\": [\n"
            + "    {\n"
            + "      \"id\": 1,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 25\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 2,\n"
            + "      \"name\": \"Tomi\",\n"
            + "      \"age\": 24\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 3,\n"
            + "      \"name\": \"Ivan\",\n"
            + "      \"age\": 26\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 4,\n"
            + "      \"name\": \"Lukas\",\n"
            + "      \"age\": 34\n"
            + "    }\n"
            + "  ]\n"
            + "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testUpdate_notExistingEntry_xml() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE id=5"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
            + "<myTable>\n"
            + "    <Entry>\n"
            + "        <id>1</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>25</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>2</id>\n"
            + "        <name>Tomi</name>\n"
            + "        <age>24</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>3</id>\n"
            + "        <name>Ivan</name>\n"
            + "        <age>26</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>4</id>\n"
            + "        <name>Lukas</name>\n"
            + "        <age>34</age>\n"
            + "    </Entry>\n"
            + "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testUpdate_moreEntries(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testUpdate_moreEntries_json();
                break;
            case "xml":
                testUpdate_moreEntries_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testUpdate_moreEntries_json() throws SQLException, IOException {
        assertEquals(3, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE name < 'Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n"
            + "  \"Entry\": [\n"
            + "    {\n"
            + "      \"id\": 1,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 25\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 2,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 24\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 3,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 26\n"
            + "    },\n"
            + "    {\n"
            + "      \"id\": 4,\n"
            + "      \"name\": \"Zsolti\",\n"
            + "      \"age\": 34\n"
            + "    }\n"
            + "  ]\n"
            + "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testUpdate_moreEntries_xml() throws SQLException, IOException {
        assertEquals(3, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE name < 'Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH), StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
            + "<myTable>\n"
            + "    <Entry>\n"
            + "        <id>1</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>25</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>2</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>24</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>3</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>26</age>\n"
            + "    </Entry>\n"
            + "    <Entry>\n"
            + "        <id>4</id>\n"
            + "        <name>Zsolti</name>\n"
            + "        <age>34</age>\n"
            + "    </Entry>\n"
            + "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

}
