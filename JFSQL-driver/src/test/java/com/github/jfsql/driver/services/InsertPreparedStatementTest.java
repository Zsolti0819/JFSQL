package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class InsertPreparedStatementTest {

    private JfsqlConnection connection;

    @BeforeEach
    void setUp() throws SQLException {
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH);
        final Statement statement = connection.createStatement();
        statement.execute("CREATE TABLE myTable ("
            + "id INTEGER NOT NULL, "
            + "name TEXT NOT NULL, "
            + "age INTEGER NOT NULL, "
            + "file BLOB)");
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testInsert_preparedStatement_simple_json() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
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

    @Test
    void testInsert_preparedStatement_simple_xml() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
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
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_preparedStatement_blob_xml() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_FILE_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "        <file>" + TestUtils.ENCODED_BLOB_PATH_XML + "</file>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent),
            StringUtils.deleteWhitespace(realFileContent));

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_FILE_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_FILE_PATH.toFile().exists());
    }

    @Test
    void testInsert_preparedStatement_blob_json() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_FILE_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "{\n" +
            "  \"Entry\": [\n" +
            "    {\n" +
            "      \"id\": 1,\n" +
            "      \"name\": \"Zsolti\",\n" +
            "      \"age\": 25,\n" +
            "      \"file\": \"" + String.valueOf(TestUtils.ENCODED_BLOB_PATH_JSON).replace("\\", "\\\\") + "\"\n" +
            "    }\n" +
            "  ]\n" +
            "}";
        assertEquals(expectedFileContent, realFileContent);

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_FILE_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_FILE_PATH.toFile().exists());
    }
}
