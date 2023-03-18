package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.io.IOException;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DropDatabaseXmlTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "xml");
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testDropDatabase() throws SQLException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertTrue(TestUtils.DATABASE_XML_FILE_PATH.toFile().exists());
        assertEquals(1, statement.executeUpdate("DROP DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertFalse(TestUtils.DATABASE_XML_FILE_PATH.toFile().exists());
    }
}
