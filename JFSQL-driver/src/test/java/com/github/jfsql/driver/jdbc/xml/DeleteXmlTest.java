package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DeleteXmlTest {

    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "xml");
        properties.setProperty("transaction.versioning", "true");
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (2, 'Tomi', 24)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (3, 'Ivan', 26)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (4, 'Lukas', 34)");
    }

    @AfterEach
    void tearDown() {
        try {
            statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
        } catch (final SQLException e) {
            TestUtils.deleteDatabaseDirectory();
        }
    }

    @Test
    void testDelete_multipleANDs() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas'"));
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
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testDelete_multipleORs() throws SQLException, IOException {
        assertEquals(3, statement.executeUpdate("DELETE FROM myTable WHERE name = 'Zsolti' OR age = 24 OR id = 3"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testDelete_multipleANDsSameEntry() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE id = 1 AND name = 'Zsolti' and age = 25"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
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
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testDelete_multipleBinaryOperators() throws SQLException, IOException {
        assertEquals(2, statement.executeUpdate(
            "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas' OR name = 'Zsolti'"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
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
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testDelete_equals() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate("DELETE FROM myTable WHERE name = 'Tomi'"));
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
    void testDelete_withoutWhere() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate("DELETE FROM myTable"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

}
