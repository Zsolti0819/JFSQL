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

class DropTableTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection(TestUtils.URL, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");
    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testDropTable_normally(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testDropTable_normally_json();
                break;
            case "xml":
                testDropTable_normally_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testDropTable_normally_json() throws SQLException {
        assertTrue(TestUtils.JSON_TABLE_PATH.toFile().exists());
        assertTrue(TestUtils.JSON_SCHEMA_PATH.toFile().exists());
        assertEquals(4, statement.executeUpdate("DROP TABLE myTable"));
        assertFalse(TestUtils.JSON_TABLE_PATH.toFile().exists());
        assertFalse(TestUtils.JSON_SCHEMA_PATH.toFile().exists());
    }

    void testDropTable_normally_xml() throws SQLException {
        assertTrue(TestUtils.XML_TABLE_PATH.toFile().exists());
        assertTrue(TestUtils.XSD_PATH.toFile().exists());
        assertEquals(4, statement.executeUpdate("DROP TABLE myTable"));
        assertFalse(TestUtils.XML_TABLE_PATH.toFile().exists());
        assertFalse(TestUtils.XSD_PATH.toFile().exists());
    }

}
