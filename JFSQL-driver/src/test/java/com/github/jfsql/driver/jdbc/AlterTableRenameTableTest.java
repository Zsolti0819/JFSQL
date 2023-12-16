package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
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

class AlterTableRenameTableTest {

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
    void testAlterTable_renameTable(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testAlterTable_renameTable_json();
                break;
            case "xml":
                testAlterTable_renameTable_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testAlterTable_renameTable_json() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable RENAME to myTableEdited;");
        final String realDatabaseFileContentAfter = FileUtils.readFileToString(new File(TestUtils.JSON_DATABASE_PATH),
            StandardCharsets.UTF_8);
        assertTrue(realDatabaseFileContentAfter.contains("myTableEdited.json"));
        assertTrue(realDatabaseFileContentAfter.contains("myTableEditedSchema.json"));
        assertFalse(realDatabaseFileContentAfter.contains("myTable.json"));
        assertFalse(realDatabaseFileContentAfter.contains("myTableSchema.json"));
        final String realTableFileContentAfter = FileUtils.readFileToString(new File(TestUtils.EDITED_JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = StringUtils.EMPTY +
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
        assertFalse(Path.of(TestUtils.JSON_SCHEMA_PATH).toFile().exists());
        assertTrue(Path.of(TestUtils.EDITED_JSON_SCHEMA_PATH).toFile().exists());
    }

    void testAlterTable_renameTable_xml() throws SQLException, IOException {
        statement.execute("ALTER TABLE myTable RENAME to myTableEdited;");
        final String realDatabaseFileContentAfter = FileUtils.readFileToString(
            Path.of(TestUtils.XML_DATABASE_PATH).toFile(),
            StandardCharsets.UTF_8);
        assertTrue(realDatabaseFileContentAfter.contains("myTableEdited.xml"));
        assertTrue(realDatabaseFileContentAfter.contains("myTableEdited.xsd"));
        assertFalse(realDatabaseFileContentAfter.contains("myTable.xml"));
        assertFalse(realDatabaseFileContentAfter.contains("myTable.xsd"));
        final String realTableFileContentAfter = FileUtils.readFileToString(
            Path.of(TestUtils.EDITED_XML_TABLE_XML_PATH).toFile(), StandardCharsets.UTF_8);
        final String expectedTableFileContentAfter = StringUtils.EMPTY +
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
        assertEquals(expectedTableFileContentAfter.replace("\n", System.lineSeparator()), realTableFileContentAfter);
        assertFalse(Path.of(TestUtils.XSD_PATH).toFile().exists());
        assertTrue(Path.of(TestUtils.EDITED_XSD_PATH).toFile().exists());
    }

}
