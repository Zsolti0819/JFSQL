package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class UpdateJdbcTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH);
        statement = connection.createStatement();
        statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (2, 'Tomi', 24)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (3, 'Ivan', 26)");
        statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (4, 'Lukas', 34)");
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testUpdate_oneEntry1_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        assertEquals(1, statement.executeUpdate(
                "UPDATE myTable SET id = 5, name = 'Marian', age=99 WHERE id = 4 AND name = 'Lukas' AND age = 34"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
                "{\n" +
                "  \"Entry\": [\n" +
                "    {\n" +
                "      \"id\": 1,\n" +
                "      \"name\": \"Zsolti\",\n" +
                "      \"age\": 25\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 2,\n" +
                "      \"name\": \"Tomi\",\n" +
                "      \"age\": 24\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 3,\n" +
                "      \"name\": \"Ivan\",\n" +
                "      \"age\": 26\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 5,\n" +
                "      \"name\": \"Marian\",\n" +
                "      \"age\": 99\n" +
                "    }\n" +
                "  ]\n" +
                "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testUpdate_oneEntry1_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        assertEquals(1, statement.executeUpdate(
                "UPDATE myTable SET id = 5, name = 'Marian', age=99 WHERE id = 4 AND name = 'Lukas' AND age = 34"));
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

    @Test
    void testUpdate_oneEntry1PreparedStatement_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        final PreparedStatement preparedStatement = connection.prepareStatement(
                "UPDATE myTable SET id = ?, name = ?, age = ? WHERE id = ? AND name = ? AND age = ?");
        preparedStatement.setInt(1, 5);
        preparedStatement.setString(2, "Marian");
        preparedStatement.setInt(3, 99);
        preparedStatement.setInt(4, 4);
        preparedStatement.setString(5, "Lukas");
        preparedStatement.setInt(6, 34);
        preparedStatement.executeUpdate();

        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
                "{\n" +
                "  \"Entry\": [\n" +
                "    {\n" +
                "      \"id\": 1,\n" +
                "      \"name\": \"Zsolti\",\n" +
                "      \"age\": 25\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 2,\n" +
                "      \"name\": \"Tomi\",\n" +
                "      \"age\": 24\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 3,\n" +
                "      \"name\": \"Ivan\",\n" +
                "      \"age\": 26\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 5,\n" +
                "      \"name\": \"Marian\",\n" +
                "      \"age\": 99\n" +
                "    }\n" +
                "  ]\n" +
                "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testUpdate_oneEntry1PreparedStatement_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        final PreparedStatement preparedStatement = connection.prepareStatement(
                "UPDATE myTable SET id = ?, name = ?, age = ? WHERE id = ? AND name = ? AND age = ?");
        preparedStatement.setInt(1, 5);
        preparedStatement.setString(2, "Marian");
        preparedStatement.setInt(3, 99);
        preparedStatement.setInt(4, 4);
        preparedStatement.setString(5, "Lukas");
        preparedStatement.setInt(6, 34);
        preparedStatement.executeUpdate();
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

    @Test
    void testUpdate_oneEntry2_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        assertEquals(1, statement.executeUpdate("UPDATE myTable SET name = 'TomiEdited' WHERE age <= 24"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
                "{\n" +
                "  \"Entry\": [\n" +
                "    {\n" +
                "      \"id\": 1,\n" +
                "      \"name\": \"Zsolti\",\n" +
                "      \"age\": 25\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 2,\n" +
                "      \"name\": \"TomiEdited\",\n" +
                "      \"age\": 24\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 3,\n" +
                "      \"name\": \"Ivan\",\n" +
                "      \"age\": 26\n" +
                "    },\n" +
                "    {\n" +
                "      \"id\": 4,\n" +
                "      \"name\": \"Lukas\",\n" +
                "      \"age\": 34\n" +
                "    }\n" +
                "  ]\n" +
                "}";
        assertEquals(expectedFileContent, realFileContent);
    }

    @Test
    void testUpdate_oneEntry2_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        assertEquals(1, statement.executeUpdate("UPDATE myTable SET name = 'TomiEdited' WHERE age <= 24"));
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
                "    <Entry>\n" +
                "        <id>2</id>\n" +
                "        <name>TomiEdited</name>\n" +
                "        <age>24</age>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>3</id>\n" +
                "        <name>Ivan</name>\n" +
                "        <age>26</age>\n" +
                "    </Entry>\n" +
                "    <Entry>\n" +
                "        <id>4</id>\n" +
                "        <name>Lukas</name>\n" +
                "        <age>34</age>\n" +
                "    </Entry>\n" +
                "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testUpdate_noWhereClauseUpdatesAll() throws SQLException {
        assertEquals(4, statement.executeUpdate("UPDATE myTable SET name='Zsolti'"));
    }

    @Test
    void testUpdate_moreEntries1() throws SQLException {
        assertEquals(3, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE age>=25"));
    }

    @Test
    void testUpdate_moreEntries2() throws SQLException {
        assertEquals(3, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE name<'Zsolti'"));
    }

    @Test
    void testUpdate_notExistingColumn() {
        assertThrows(SQLException.class, () -> statement.executeUpdate("UPDATE myTable SET asd='Zsolti' WHERE id=4"));
    }

    @Test
    void testUpdate_notExistingEntry() throws SQLException {
        assertEquals(0, statement.executeUpdate("UPDATE myTable SET name='Zsolti' WHERE id=5"));
    }
}