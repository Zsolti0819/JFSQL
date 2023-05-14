package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class UpdatePreparedStatementTest {

    private Connection connection;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection(TestUtils.URL, properties);
        final Statement statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");

    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testUpdate_oneEntryPreparedStatement(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testUpdate_oneEntryPreparedStatement_json();
                break;
            case "xml":
                testUpdate_oneEntryPreparedStatement_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testUpdate_oneEntryPreparedStatement_json() throws SQLException, IOException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "UPDATE myTable SET id = ?, name = ?, age = ?, file = ? WHERE id = ? AND name = ? AND age = ?");
        preparedStatement.setInt(1, 5);
        preparedStatement.setString(2, "Marian");
        preparedStatement.setInt(3, 99);
        preparedStatement.setBinaryStream(4, null);
        preparedStatement.setInt(5, 4);
        preparedStatement.setString(6, "Lukas");
        preparedStatement.setInt(7, 34);
        preparedStatement.executeUpdate();

        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": null\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 2,\n" +
            "      \"name\": \"Tomi\",\n" +
            "      \"age\": 24,\n" +
            "      \"file\": null\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 3,\n" +
            "      \"name\": \"Ivan\",\n" +
            "      \"age\": 26,\n" +
            "      \"file\": null\n" +
            "    },\n" +
            "    {\n" +
            "      \"id\": 5,\n" +
            "      \"name\": \"Marian\",\n" +
            "      \"age\": 99,\n" +
            "      \"file\": null\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testUpdate_oneEntryPreparedStatement_xml() throws SQLException, IOException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "UPDATE myTable SET id = ?, name = ?, age = ?, file = ? WHERE id = ? AND name = ? AND age = ?");
        preparedStatement.setInt(1, 5);
        preparedStatement.setString(2, "Marian");
        preparedStatement.setInt(3, 99);
        preparedStatement.setBinaryStream(4, null);
        preparedStatement.setInt(5, 4);
        preparedStatement.setString(6, "Lukas");
        preparedStatement.setInt(7, 34);
        preparedStatement.executeUpdate();
        
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
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
            "    <Entry>\n" +
            "        <id>5</id>\n" +
            "        <name>Marian</name>\n" +
            "        <age>99</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

}
