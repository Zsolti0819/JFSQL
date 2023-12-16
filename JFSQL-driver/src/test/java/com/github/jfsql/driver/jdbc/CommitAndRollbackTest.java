package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.io.File;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

public class CommitAndRollbackTest {

    private Connection connection;

    private void setup(final String persistence) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", "jgit");
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection(TestUtils.URL, properties);
        final Statement statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "xml"})
    void testCommitAndRollback(final String persistence)
        throws SQLException, IOException {
        setup(persistence);
        switch (persistence) {
            case "json":
                testCommitAndRollback_json();
                break;
            case "xml":
                testCommitAndRollback_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testCommitAndRollback_json() throws SQLException, IOException {
        connection.setAutoCommit(false);
        final String realFileContentBefore = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContentBefore = StringUtils.EMPTY +
            "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedFileContentBefore, realFileContentBefore);

        final FileWriter fileWriter = new FileWriter(TestUtils.JSON_TABLE_PATH, false);
        fileWriter.write("test");
        fileWriter.close();

        final String realFileContentAfterModification = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        assertEquals("test", realFileContentAfterModification);

        connection.rollback();

        final String realFileContentAfterRollback = FileUtils.readFileToString(new File(TestUtils.JSON_TABLE_PATH),
            StandardCharsets.UTF_8);
        assertEquals(realFileContentBefore.replace("\n", System.lineSeparator()), realFileContentAfterRollback);

    }

    void testCommitAndRollback_xml() throws SQLException, IOException {
        connection.setAutoCommit(false);
        final String realFileContentBefore = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        final String expectedFileContentBefore = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(expectedFileContentBefore.replace("\n", System.lineSeparator()), realFileContentBefore);

        final FileWriter fileWriter = new FileWriter(TestUtils.XML_TABLE_PATH, false);
        fileWriter.write("test");
        fileWriter.close();

        final String realFileContentAfterModification = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        assertEquals("test", realFileContentAfterModification);

        connection.rollback();

        final String realFileContentAfterRollback = FileUtils.readFileToString(new File(TestUtils.XML_TABLE_PATH),
            StandardCharsets.UTF_8);
        assertEquals(realFileContentBefore, realFileContentAfterRollback);

    }

}
