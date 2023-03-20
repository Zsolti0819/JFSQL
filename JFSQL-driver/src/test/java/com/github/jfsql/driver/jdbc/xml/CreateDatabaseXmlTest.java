package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import java.io.File;
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

class CreateDatabaseXmlTest {

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
    void testCreateDatabase_normally() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE2_PATH + "];"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.DATABASE2_XML_FILE_PATH.toUri()),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<Database name=\"myDatabase2\"/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE2_PATH + "]");
    }

}
