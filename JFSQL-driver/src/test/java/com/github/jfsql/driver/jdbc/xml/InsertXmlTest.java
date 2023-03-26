package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class InsertXmlTest {

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
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
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
    void testInsert_simple() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
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
    void testInsert_differentColumnOrder() throws SQLException, IOException {
        assertEquals(1, statement.executeUpdate(
            "INSERT INTO myTable (name, id, age) VALUES ('Zsolti', 1, 25)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
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
    void testInsert_preparedStatement_simple() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
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
    void testInsert_preparedStatement_differentColumnOrder() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (name, id, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setString(1, "Zsolti");
        preparedStatement.setInt(2, 1);
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
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
    void testInsert_preparedStatement_blob() throws SQLException, IOException {
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
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

    @Test
    void testInsert_multiRow() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
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
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_noExplicitColumns() throws SQLException, IOException {
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
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
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_multiple() throws SQLException {
        assertEquals(2,
            statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24)"));
        assertEquals(3, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24)"));
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals(2, statement.executeUpdate("INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24)"));
        assertEquals(3,
            statement.executeUpdate("INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24)"));
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
    }

}
