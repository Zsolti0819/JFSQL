package com.github.jfsql.driver.services;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class CreateDatabaseJdbcTest {

    private JfsqlConnection connection;
    private Statement statement;

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
    void testCreateDatabase_normally_json() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof WriterJsonImpl);
        assertEquals(0, statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE2_PATH + "];"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.DATABASE2_JSON_FILE_PATH.toUri()),
                StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
                "{\n" +
                "  \"Database\": \"myDatabase2\",\n" +
                "  \"Table\": []\n" +
                "}";
        assertEquals(expectedFileContent, realFileContent);
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE2_PATH + "]");
    }

    @Test
    void testCreateDatabase_normally_xml() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof WriterXmlImpl);
        assertEquals(0, statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE2_PATH + "];"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.DATABASE2_XML_FILE_PATH.toUri()),
                StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
                "<Database name=\"myDatabase2\"/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE2_PATH + "]");
    }

    @Test
    void testCreateDatabase_databaseIsNotDirectory() {
        final SQLException thrown = assertThrows(SQLException.class,
                () -> statement.executeUpdate("CREATE DATABASE [" + TestUtils.NOT_DIRECTORY_PATH + "];"));
        assertEquals("Database is not a directory.", thrown.getMessage());
    }

    @Test
    void testCreateDatabase_databaseExists() {
        final SQLException thrown = assertThrows(SQLException.class,
                () -> statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertEquals("Database already exists, will not create another one.", thrown.getMessage());

    }
}
