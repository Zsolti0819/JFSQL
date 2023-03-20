package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import java.io.FileWriter;
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

class TransactionManagerXmlTest {

    private Statement statement;
    private Connection connection;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "xml");
        properties.setProperty("transaction.versioning", "true");
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
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
    void testCommit_xml() throws SQLException, IOException {
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);
        statement.execute("INSERT INTO myTable VALUES (1, 'a', 25)");

        // Inserted, but not yet committed, so the table looks the same
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent),
            StringUtils.deleteWhitespace(realFileContent));

        connection.commit();

        // After the commit, changes are written to the file
        final String realFileContent2 = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent2 = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
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

    @Test
    void testCommitDropTable_xml() throws SQLException {
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);

        // Table's files were created
        assertTrue(TestUtils.TABLE_XML_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_XSD_FILE_PATH.toFile().exists());

        statement.execute("DROP TABLE myTable;");

        // Everything exist, because it wasn't committed
        assertTrue(TestUtils.TABLE_XML_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_XSD_FILE_PATH.toFile().exists());

        connection.commit();

        // After the commit the files doesn't exist
        assertFalse(TestUtils.TABLE_XML_FILE_PATH.toFile().exists());
        assertFalse(TestUtils.TABLE_XSD_FILE_PATH.toFile().exists());

    }

    @Test
    void testCommitAndRollback_xml() throws SQLException, IOException {
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);

        final String realFileContentBefore = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContentBefore = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContentBefore),
            StringUtils.deleteWhitespace(realFileContentBefore));

        final FileWriter fileWriter = new FileWriter(TestUtils.TABLE_XML_FILE_PATH.toFile(), false);
        fileWriter.write("test");
        fileWriter.close();

        final String realFileContentAfterModification = FileUtils.readFileToString(
            TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        assertEquals(StringUtils.deleteWhitespace("test"),
            StringUtils.deleteWhitespace(realFileContentAfterModification));

        connection.rollback();

        final String realFileContentAfterRollback = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        assertEquals(StringUtils.deleteWhitespace(realFileContentBefore),
            StringUtils.deleteWhitespace(realFileContentAfterRollback));

    }

}
