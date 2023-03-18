package com.github.jfsql.driver.jdbc.json;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CreateDatabaseJsonTest {

    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "json");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testCreateDatabase_normally() throws SQLException, IOException {
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
