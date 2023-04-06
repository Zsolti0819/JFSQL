package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class DropDatabaseTest {

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
    void testDropDatabase(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDropDatabase_json();
                break;
            case "xml":
                testDropDatabase_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDropDatabase_json() throws SQLException {
        assertTrue(TestUtils.JSON_DATABASE_PATH.toFile().exists());
        assertEquals(1, statement.executeUpdate("DROP DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertFalse(TestUtils.JSON_DATABASE_PATH.toFile().exists());
    }

    void testDropDatabase_xml() throws SQLException {
        assertTrue(TestUtils.XML_DATABASE_PATH.toFile().exists());
        assertEquals(1, statement.executeUpdate("DROP DATABASE [" + TestUtils.DATABASE_PATH + "];"));
        assertFalse(TestUtils.XML_DATABASE_PATH.toFile().exists());
    }

}
