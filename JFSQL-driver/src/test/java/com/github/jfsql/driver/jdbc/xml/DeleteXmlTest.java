package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class DeleteXmlTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "xml");
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
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
    void testDelete_columnsNotExist() {
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.execute("DELETE FROM myTable WHERE lol > 3 AND age > 25 AND name = 'Lukas'"));
        assertEquals("Some columns entered doesn't exist in \"myTable\".", thrown.getMessage());

    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas'"
    })
    void testDelete_multipleANDs(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(1, statement.executeUpdate(sql));
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

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE name = 'Zsolti' OR age = 24 OR id = 3 OR name = 'Lukas'"
    })
    void testDelete_multipleORs(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(4, statement.executeUpdate(
            sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE id = 1 AND name = 'Zsolti' and age = 25"
    })
    void testDelete_multipleANDsSameEntry(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(1, statement.executeUpdate(sql));
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

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE id > 3 AND age > 25 AND name = 'Lukas' OR name = 'Zsolti'"
    })
    void testDelete_multipleBinaryOperators(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(2, statement.executeUpdate(sql));
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

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE name = 'Tomi'"
    })
    void testDelete_equals(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(1, statement.executeUpdate(sql));
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

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable WHERE age <= 34"
    })
    void testDelete_lte(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(4, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "DELETE FROM myTable"
    })
    void testDelete_withoutWhere(final String sql) throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(4, statement.executeUpdate(sql));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }
}
