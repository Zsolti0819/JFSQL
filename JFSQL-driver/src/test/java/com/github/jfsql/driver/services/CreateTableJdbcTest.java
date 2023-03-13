package com.github.jfsql.driver.services;

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
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class CreateTableJdbcTest {

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
    void testCreateTable_normally_json() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterJsonImpl);
        assertEquals(0, statement.executeUpdate(
                "CREATE TABLE myTable (id INTEGER NOT NULL, name TEXT NOT NULL, age INTEGER NOT NULL)"));
        final String realTableFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedTableFileContent = "" +
                "{\n" +
                "  \"Entry\": []\n" +
                "}";
        assertEquals(expectedTableFileContent, realTableFileContent);
        final String realSchemaFileContent = FileUtils.readFileToString(TestUtils.TABLE_JSON_SCHEMA_FILE_PATH.toFile(),
                StandardCharsets.UTF_8);
        final String expectedSchemaFileContent = "" +
                "{\n" +
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

    @Test
    void testCreateTable_normally_xml() throws SQLException, IOException {
        assumeTrue(connection.getWriter() instanceof WriterXmlImpl);
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

    @Test
    void testCreateTable_tableNameAndDatabaseNameAreEqual() {
        final SQLException thrown = assertThrows(SQLException.class,
                () -> statement.executeUpdate("CREATE TABLE myDatabase (id INTEGER, name TEXT, age INTEGER)"));
        assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
    }

    @Test
    void testCreateTable_tableExists() throws SQLException {
        assertEquals(0, statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)"));
        final SQLException thrown = assertThrows(SQLException.class,
                () -> statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)"));
        assertEquals("Table \"myTable\" already exists.", thrown.getMessage());
    }

    @Test
    void testCreateTable_ifNotExists_doesNotThrowException() throws SQLException {
        assertEquals(0, statement.executeUpdate("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)"));
        assertDoesNotThrow(() -> statement.executeUpdate("CREATE TABLE IF NOT EXISTS myTable (id INTEGER, name TEXT, age INTEGER)"));
    }

    @Test
    void testCreateTable_duplicateColumns() {
        final SQLException thrown = assertThrows(SQLException.class,
                () -> statement.executeUpdate("CREATE TABLE myTable (name TEXT, name TEXT, age INTEGER)"));
        assertEquals("Some columns were identical during table creation.", thrown.getMessage());
    }

}
