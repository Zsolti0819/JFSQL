package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class TransactionManagerTest {

    private Statement statement;
    private JfsqlConnection connection;

    @BeforeEach
    void setUp() throws SQLException {
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH);
        statement = connection.createStatement();
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testCommit_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);
        connection.commit();
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
    void testCommit_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);
        connection.commit();
        statement.execute("INSERT INTO myTable VALUES (1, 'a', 25)");

        // Inserted, but not yet committed, so the table looks the same
        final String firstCommitRealFileContent = FileUtils.readFileToString(
                TestUtils.TABLE_JSON_FILE_PATH.toFile(), StandardCharsets.UTF_8);
        final String firstCommitExpectedFileContent = "" +
                "{\n" +
                "  \"Entry\": []\n" +
                "}";
        assertEquals(firstCommitExpectedFileContent, firstCommitRealFileContent);

        connection.commit();

        // After the commit, changes are written to the file
        final String secondCommitRealFileContent = FileUtils.readFileToString(
                TestUtils.TABLE_JSON_FILE_PATH.toFile(), StandardCharsets.UTF_8);
        final String secondCommitExpectedFileContent = "" +
                "{\n" +
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

    @Test
    void testCommitDropTable_xml() throws SQLException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        assumeTrue(connection.getTransactionManager() instanceof JGitTransactionManagerImpl);
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
    void testCommitDropTable_json() throws SQLException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        assumeTrue(connection.getTransactionManager() instanceof JGitTransactionManagerImpl);
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);

        // Table's files were created
        assertTrue(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());

        statement.execute("DROP TABLE myTable;");

        // Everything exist, because it wasn't committed
        assertTrue(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());

        connection.commit();

        // After the commit the files doesn't exist
        assertFalse(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertFalse(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());

    }

    @Test
    void testCommitAndRollback_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        assumeTrue(connection.getTransactionManager() instanceof JGitTransactionManagerImpl);
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);

        final String realFileContentBefore = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedFileContentBefore = "" +
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
                "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContentBefore), StringUtils.deleteWhitespace(realFileContentBefore));

        final FileWriter fileWriter = new FileWriter(TestUtils.TABLE_XML_FILE_PATH.toFile(), false);
        fileWriter.write("test");
        fileWriter.close();

        final String realFileContentAfterModification = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        assertEquals(StringUtils.deleteWhitespace("test"), StringUtils.deleteWhitespace(realFileContentAfterModification));

        connection.rollback();

        final String realFileContentAfterRollback = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        assertEquals(StringUtils.deleteWhitespace(realFileContentBefore), StringUtils.deleteWhitespace(realFileContentAfterRollback));

    }

    @Test
    void testCommitAndRollback_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        assumeTrue(connection.getTransactionManager() instanceof JGitTransactionManagerImpl);
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        connection.setAutoCommit(false);

        final String realFileContentBefore = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedFileContentBefore = "" +
                "{\n" +
                "  \"Entry\": []\n" +
                "}";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContentBefore), StringUtils.deleteWhitespace(realFileContentBefore));

        final FileWriter fileWriter = new FileWriter(TestUtils.TABLE_JSON_FILE_PATH.toFile(), false);
        fileWriter.write("test");
        fileWriter.close();

        final String realFileContentAfterModification = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        assertEquals(StringUtils.deleteWhitespace("test"), StringUtils.deleteWhitespace(realFileContentAfterModification));

        connection.rollback();

        final String realFileContentAfterRollback = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        assertEquals(StringUtils.deleteWhitespace(realFileContentBefore), StringUtils.deleteWhitespace(realFileContentAfterRollback));

    }

}
