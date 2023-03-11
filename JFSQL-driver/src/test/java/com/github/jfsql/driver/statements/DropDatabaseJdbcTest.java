package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class DropDatabaseJdbcTest {

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
    void testDropDatabase_json() throws SQLException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        assertTrue(TestUtils.DATABASE_JSON_FILE_PATH.toFile().exists());
        assertEquals(1, statement.executeUpdate("DROP DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertFalse(TestUtils.DATABASE_JSON_FILE_PATH.toFile().exists());
    }

    @Test
    void testDropDatabase_xml() throws SQLException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
        assertTrue(TestUtils.DATABASE_XML_FILE_PATH.toFile().exists());
        assertEquals(1, statement.executeUpdate("DROP DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertFalse(TestUtils.DATABASE_XML_FILE_PATH.toFile().exists());
    }
}
