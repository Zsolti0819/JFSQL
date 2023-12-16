package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class DeleteTest {

    private Statement statement;
    private Connection connection;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection(TestUtils.URL, properties);
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
    void testDelete_withoutWhereDeletesAll(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_withoutWhereDeletesAll_json();
                break;
            case "xml":
                testDelete_withoutWhereDeletesAll_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_withoutWhereDeletesAll_json() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate("DELETE FROM myTable"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testDelete_withoutWhereDeletesAll_xml() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate("DELETE FROM myTable"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(expectedFileContent.replace("\n", System.lineSeparator()), realFileContent);
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDelete_equals(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_equals_json();
                break;
            case "xml":
                testDelete_equals_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_equals_json() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE name = 'Tomi'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25\n" +
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

    void testDelete_equals_xml() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE name = 'Tomi'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
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
        assertEquals(expectedFileContent.replace("\n", System.lineSeparator()), realFileContent);
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDelete_multipleBinaryOperators(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_multipleBinaryOperators_json();
                break;
            case "xml":
                testDelete_multipleBinaryOperators_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_multipleBinaryOperators_json() throws SQLException, IOException {
        assertEquals(2, statement.executeUpdate(
            "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas' OR name = 'Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testDelete_multipleBinaryOperators_xml() throws SQLException, IOException {
        assertEquals(2, statement.executeUpdate(
            "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas' OR name = 'Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
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
            "</myTable>\n";
        assertEquals(expectedFileContent.replace("\n", System.lineSeparator()), realFileContent);
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDelete_multipleANDsSameEntry(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_multipleANDsSameEntry_json();
                break;
            case "xml":
                testDelete_multipleANDsSameEntry_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_multipleANDsSameEntry_json() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE id = 1 AND name = 'Zsolti' and age = 25"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": [\n" +
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
        assertEquals(expectedFileContent, realFileContent);
    }

    void testDelete_multipleANDsSameEntry_xml() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE id = 1 AND name = 'Zsolti' and age = 25"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
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
        assertEquals(expectedFileContent.replace("\n", System.lineSeparator()), realFileContent);
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDelete_multipleORs(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_multipleORs_json();
                break;
            case "xml":
                testDelete_multipleORs_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_multipleORs_json() throws SQLException, IOException {
        assertEquals(3, statement.executeUpdate("DELETE FROM myTable WHERE name = 'Zsolti' OR age = 24 OR id = 3"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n"
            + "  \"Entry\": [\n"
            + "    {\n"
            + "      \"id\": 4,\n"
            + "      \"name\": \"Lukas\",\n"
            + "      \"age\": 34\n"
            + "    }\n"
            + "  ]\n"
            + "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testDelete_multipleORs_xml() throws SQLException, IOException {
        assertEquals(3, statement.executeUpdate("DELETE FROM myTable WHERE name = 'Zsolti' OR age = 24 OR id = 3"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(expectedFileContent.replace("\n", System.lineSeparator()), realFileContent);
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDelete_multipleANDs(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_multipleANDs_json();
                break;
            case "xml":
                testDelete_multipleANDs_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_multipleANDs_json() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
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
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testDelete_multipleANDs_xml() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas'"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
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
            "</myTable>\n";
        assertEquals(expectedFileContent.replace("\n", System.lineSeparator()), realFileContent);
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDelete_blob(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDelete_BLOB_json();
                break;
            case "xml":
                testDelete_BLOB_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDelete_BLOB_json() throws SQLException, FileNotFoundException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH));
        assertEquals(1, preparedStatement.executeUpdate());
        assertTrue(Path.of(TestUtils.ENCODED_JSON_BLOB_PATH).toFile().exists());

        statement.executeUpdate("DELETE FROM myTable WHERE id = 1");

        assertFalse(Path.of(TestUtils.ENCODED_JSON_BLOB_PATH).toFile().exists());
    }

    void testDelete_BLOB_xml() throws SQLException, FileNotFoundException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH));
        assertEquals(1, preparedStatement.executeUpdate());
        assertTrue(Path.of(TestUtils.ENCODED_XML_BLOB_PATH).toFile().exists());

        statement.executeUpdate("DELETE FROM myTable WHERE id = 1");

        assertFalse(Path.of(TestUtils.ENCODED_XML_BLOB_PATH).toFile().exists());
    }
}
