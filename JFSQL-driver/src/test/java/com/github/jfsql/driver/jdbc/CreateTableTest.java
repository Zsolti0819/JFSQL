package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class CreateTableTest {

    private Statement statement;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        final Connection connection = DriverManager.getConnection(TestUtils.URL, properties);
        statement = connection.createStatement();
    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testCreateDatabase_normally(final String persistence, final String transactionVersioning)
        throws SQLException, IOException {
        setup(persistence, transactionVersioning);
        switch (persistence) {
            case "json":
                testCreateTable_normally_json();
                break;
            case "xml":
                testCreateTable_normally_xml();
                break;
            default:
                fail("Unexpected value: " + persistence);
        }
    }

    void testCreateTable_normally_json() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate(
            "CREATE TABLE myTable (id INTEGER NOT NULL, name TEXT NOT NULL, age INTEGER NOT NULL)"));
        final String realTableFileContent = FileUtils.readFileToString(TestUtils.JSON_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContent = "{\n" +
            "  \"Entry\": []\n" +
            "}";
        assertEquals(expectedTableFileContent, realTableFileContent);
        final String realSchemaFileContent = FileUtils.readFileToString(TestUtils.JSON_SCHEMA_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedSchemaFileContent = "{\n" +
            "  \"$schema\": \"http://json-schema.org/draft-06/schema#\",\n" +
            "  \"type\": \"object\",\n" +
            "  \"required\": [\n" +
            "    \"Entry\"\n" +
            "  ],\n" +
            "  \"properties\": {\n" +
            "    \"Entry\": {\n" +
            "      \"type\": \"array\",\n" +
            "      \"items\": {\n" +
            "        \"type\": \"object\",\n" +
            "        \"required\": [\n" +
            "          \"id\",\n" +
            "          \"name\",\n" +
            "          \"age\"\n" +
            "        ],\n" +
            "        \"properties\": {\n" +
            "          \"id\": {\n" +
            "            \"type\": [\n" +
            "              \"integer\"\n" +
            "            ]\n" +
            "          },\n" +
            "          \"name\": {\n" +
            "            \"type\": [\n" +
            "              \"string\"\n" +
            "            ]\n" +
            "          },\n" +
            "          \"age\": {\n" +
            "            \"type\": [\n" +
            "              \"integer\"\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      }\n" +
            "    }\n" +
            "  }\n" +
            "}";
        assertEquals(expectedSchemaFileContent, realSchemaFileContent);
    }

    void testCreateTable_normally_xml() throws SQLException, IOException {
        assertEquals(0, statement.executeUpdate(
            "CREATE TABLE myTable (id INTEGER NOT NULL, name TEXT NOT NULL, age INTEGER NOT NULL)"));
        final String realTableFileContent = FileUtils.readFileToString(TestUtils.XML_TABLE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedTableFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable/>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedTableFileContent),
            StringUtils.deleteWhitespace(realTableFileContent));
        final String realSchemaFileContent = FileUtils.readFileToString(TestUtils.XSD_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedSchemaFileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
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
