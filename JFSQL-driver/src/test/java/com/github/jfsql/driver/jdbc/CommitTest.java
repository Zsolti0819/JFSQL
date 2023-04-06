package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
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

public class CommitTest {

    private Statement statement;
    private Connection connection;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @AfterEach
    void tearDown() {
        try {
            statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
        } catch (final SQLException e) {
            TestUtils.deleteDatabaseDirectory();
        }
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCommit(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testCommit_json();
                break;
            case "xml":
                testCommit_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testCommit_json() throws SQLException, IOException {
        connection.setAutoCommit(false);
        statement.execute("INSERT INTO myTable VALUES (1, 'a', 25)");

        // Inserted, but not yet committed, so the table looks the same
        final String firstCommitRealFileContent = FileUtils.readFileToString(
            TestUtils.JSON_TABLE_PATH.toFile(), StandardCharsets.UTF_8);
        final String firstCommitExpectedFileContent = "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(firstCommitExpectedFileContent, firstCommitRealFileContent);

        connection.commit();

        // After the commit, changes are written to the file
        final String secondCommitRealFileContent = FileUtils.readFileToString(
            TestUtils.JSON_TABLE_PATH.toFile(), StandardCharsets.UTF_8);
        final String secondCommitExpectedFileContent = "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"a\",\n" +
            "      \"age\": 25\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(secondCommitExpectedFileContent, secondCommitRealFileContent);

    }

    void testCommit_xml() throws SQLException, IOException {
        connection.setAutoCommit(false);
        statement.execute("INSERT INTO myTable VALUES (1, 'a', 25)");

        // Inserted, but not yet committed, so the table looks the same
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent),
            StringUtils.deleteWhitespace(realFileContent));

        connection.commit();

        // After the commit, changes are written to the file
        final String realFileContent2 = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent2 = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>a</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent2),
            StringUtils.deleteWhitespace(realFileContent2));

    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCommitDropTable_json(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testCommitDropTable_json();
                break;
            case "xml":
                testCommitDropTable_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testCommitDropTable_json() throws SQLException {
        connection.setAutoCommit(false);

        // Table's files were created
        assertTrue(TestUtils.JSON_TABLE_PATH.toFile().exists());
        assertTrue(TestUtils.JSON_SCHEMA_PATH.toFile().exists());

        statement.execute("DROP TABLE myTable;");

        // Everything exist, because it wasn't committed
        assertTrue(TestUtils.JSON_TABLE_PATH.toFile().exists());
        assertTrue(TestUtils.JSON_SCHEMA_PATH.toFile().exists());

        connection.commit();

        // After the commit the files don't exist
        assertFalse(TestUtils.JSON_TABLE_PATH.toFile().exists());
        assertFalse(TestUtils.JSON_SCHEMA_PATH.toFile().exists());

    }

    void testCommitDropTable_xml() throws SQLException {
        connection.setAutoCommit(false);

        // Table's files were created
        assertTrue(TestUtils.XML_TABLE_PATH.toFile().exists());
        assertTrue(TestUtils.XSD_PATH.toFile().exists());

        statement.execute("DROP TABLE myTable;");

        // Everything exist, because it wasn't committed
        assertTrue(TestUtils.XML_TABLE_PATH.toFile().exists());
        assertTrue(TestUtils.XSD_PATH.toFile().exists());

        connection.commit();

        // After the commit the files don't exist
        assertFalse(TestUtils.XML_TABLE_PATH.toFile().exists());
        assertFalse(TestUtils.XSD_PATH.toFile().exists());

    }

}
