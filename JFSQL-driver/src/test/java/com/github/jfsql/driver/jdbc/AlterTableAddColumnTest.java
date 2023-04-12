package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
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

class AlterTableAddColumnTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
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
    void testAlterTable_addColumn(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testAlterTable_addColumn_json();
                break;
            case "xml":
                testAlterTable_addColumn_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testAlterTable_addColumn_json() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = StringUtils.EMPTY +
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

    void testAlterTable_addColumn_xml() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = StringUtils.EMPTY +
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

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testAlterTable_addNotNullColumn(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testAlterTable_addNotNullColumn_json();
                break;
            case "xml":
                testAlterTable_addNotNullColumn_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testAlterTable_addNotNullColumn_json() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL NOT NULL;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"salary\": 0.0\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24,\n" +
            "      \"salary\": 0.0\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26,\n" +
            "      \"salary\": 0.0\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 4,\n" +
            "      \"name\": \"Lukas\",\n" +
            "      \"age\": 34,\n" +
            "      \"salary\": 0.0\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedTableFileContentAfter, realTableFileContentAfter);
    }

    void testAlterTable_addNotNullColumn_xml() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable ADD COLUMN salary REAL NOT NULL;");
        final String realTableFileContentAfter = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "        <salary>0</salary>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>2</id>\n" +
            "        <name>Tomi</name>\n" +
            "        <age>24</age>\n" +
            "        <salary>0</salary>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>3</id>\n" +
            "        <name>Ivan</name>\n" +
            "        <age>26</age>\n" +
            "        <salary>0</salary>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "        <salary>0</salary>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContentAfter),
            StringUtils.deleteWhitespace(realTableFileContentAfter));
    }
}
