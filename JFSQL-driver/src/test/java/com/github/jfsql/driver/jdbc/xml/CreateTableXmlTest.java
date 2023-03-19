package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CreateTableXmlTest {

    private static Statement statement;

    @AfterAll
    static void afterAll() throws SQLException {
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
    }

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "xml");
        final Connection connection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
    }

    @Test
    void testCreateTable_normally() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate(
            "CREATE TABLE myTable (id INTEGER NOT NULL, name TEXT NOT NULL, age INTEGER NOT NULL)"));
        final String realTableFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContent),
            StringUtils.deleteWhitespace(realTableFileContent));
        final String realSchemaFileContent = FileUtils.readFileToString(TestUtils.TABLE_XSD_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedSchemaFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">\n" +
            "    <xs:element name=\"myTable\">\n" +
            "        <xs:complexType>\n" +
            "            <xs:sequence>\n" +
            "                <xs:element maxOccurs=\"unbounded\" minOccurs=\"0\" name=\"Entry\">\n" +
            "                    <xs:complexType>\n" +
            "                        <xs:sequence>\n" +
            "                            <xs:element name=\"id\" type=\"xs:long\"/>\n" +
            "                            <xs:element name=\"name\" type=\"xs:string\"/>\n" +
            "                            <xs:element name=\"age\" type=\"xs:long\"/>\n" +
            "                        </xs:sequence>\n" +
            "                    </xs:complexType>\n" +
            "                </xs:element>\n" +
            "            </xs:sequence>\n" +
            "        </xs:complexType>\n" +
            "    </xs:element>\n" +
            "</xs:schema>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedSchemaFileContent),
            StringUtils.deleteWhitespace(realSchemaFileContent));

    }

}
