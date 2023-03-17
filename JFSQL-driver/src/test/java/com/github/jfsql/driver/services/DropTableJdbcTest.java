package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.io.IOException;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DropTableJdbcTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH);
        statement = connection.createStatement();
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testDropTable_normally_json() throws SQLException {
        assumeTrue(connection.getReader() instanceof ReaderJsonImpl);
        assertTrue(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());
        assertFalse(statement.execute("DROP TABLE myTable"));
        assertFalse(TestUtils.TABLE_JSON_FILE_PATH.toFile().exists());
        assertFalse(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile().exists());
    }

    @Test
    void testDropTable_normally_xml() throws SQLException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertTrue(TestUtils.TABLE_XML_FILE_PATH.toFile().exists());
        assertTrue(TestUtils.TABLE_XSD_FILE_PATH.toFile().exists());
        assertFalse(statement.execute("DROP TABLE myTable"));
        assertFalse(TestUtils.TABLE_XML_FILE_PATH.toFile().exists());
        assertFalse(TestUtils.TABLE_XSD_FILE_PATH.toFile().exists());
    }

    @Test
    void testDropTable_correctEntryCount() throws SQLException {
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)");
        assertEquals(4, statement.executeUpdate("DROP TABLE myTable"));
    }
}