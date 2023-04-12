package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

public class InsertPreparedStatementTest {

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
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");

    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testInsert_preparedStatement_simple(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testInsert_preparedStatement_simple_json();
                break;
            case "xml":
                testInsert_preparedStatement_simple_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testInsert_preparedStatement_simple_json() throws SQLException, IOException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": null\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testInsert_preparedStatement_simple_xml() throws SQLException, IOException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testInsert_preparedStatement_differentColumnOrder(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testInsert_preparedStatement_differentColumnOrder_json();
                break;
            case "xml":
                testInsert_preparedStatement_differentColumnOrder_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testInsert_preparedStatement_differentColumnOrder_json() throws SQLException, IOException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (name, id, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setString(1, "Zsolti");
        preparedStatement.setInt(2, 1);
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": null\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    void testInsert_preparedStatement_differentColumnOrder_xml() throws SQLException, IOException {
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (name, id, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setString(1, "Zsolti");
        preparedStatement.setInt(2, 1);
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testInsert_preparedStatement_blob(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testInsert_preparedStatement_blob_json();
                break;
            case "xml":
                testInsert_preparedStatement_blob_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testInsert_preparedStatement_blob_json() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());
        assertTrue(TestUtils.ENCODED_JSON_BLOB_PATH.toFile().exists());

        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": \"" + String.valueOf(TestUtils.ENCODED_JSON_BLOB_PATH).replace("\\", "\\\\") + "\"\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_PATH.toFile().exists());
    }

    void testInsert_preparedStatement_blob_xml() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());
        assertTrue(TestUtils.ENCODED_XML_BLOB_PATH.toFile().exists());

        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "        <file>" + TestUtils.ENCODED_XML_BLOB_PATH + "</file>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent),
            StringUtils.deleteWhitespace(realFileContent));

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_PATH.toFile().exists());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testInsert_preparedStatement_blob_transaction(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testInsert_preparedStatement_blob_transaction_json();
                break;
            case "xml":
                testInsert_preparedStatement_blob_transaction_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testInsert_preparedStatement_blob_transaction_json() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        connection.setAutoCommit(false);
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        connection.commit();

        final String realFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": \"" + String.valueOf(TestUtils.ENCODED_JSON_BLOB_PATH).replace("\\", "\\\\") + "\"\n" +
            "    }\n" +
            "  ]\n" +
            "}";

        assertEquals(expectedFileContent, realFileContent);

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_PATH.toFile().exists());

        statement.execute("DELETE FROM myTable WHERE id = 1");

        connection.commit();

        assertFalse(TestUtils.ENCODED_JSON_BLOB_PATH.toFile().exists());
    }

    void testInsert_preparedStatement_blob_transaction_xml() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        connection.setAutoCommit(false);
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        connection.commit();

        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = StringUtils.EMPTY +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "        <file>" + TestUtils.ENCODED_XML_BLOB_PATH + "</file>\n" +
            "    </Entry>\n" +
            "</myTable>\n";

        assertEquals(StringUtils.deleteWhitespace(expectedFileContent),
            StringUtils.deleteWhitespace(realFileContent));

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_PATH.toFile().exists());

        statement.execute("DELETE FROM myTable WHERE id = 1");

        connection.commit();

        assertFalse(TestUtils.ENCODED_XML_BLOB_PATH.toFile().exists());
    }

}
