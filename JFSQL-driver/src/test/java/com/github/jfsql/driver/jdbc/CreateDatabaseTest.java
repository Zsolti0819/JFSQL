package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class CreateDatabaseTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
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

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateDatabase_normally(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testCreateDatabase_normally_json();
                break;
            case "xml":
                testCreateDatabase_normally_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testCreateDatabase_normally_json() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE2_PATH + "];"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.JSON_DATABASE2_PATH.toUri()),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "{\n" +
            "  \"Database\": \"myDatabase2\",\n" +
            "  \"Table\": []\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE2_PATH + "]");
    }

    void testCreateDatabase_normally_xml() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate("CREATE DATABASE [" + TestUtils.DATABASE2_PATH + "];"));
        final String realFileContent = FileUtils.readFileToString(new File(TestUtils.XML_DATABASE2_PATH.toUri()),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<Database name=\"myDatabase2\"/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE2_PATH + "]");
    }

}
