package com.github.jfsql.driver.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class FileNameCreatorTest {

    @Mock
    private Database database;

    @Test
    void testCreateTableFileName_json() {
        when(database.getURL()).thenReturn(String.valueOf(TestUtils.JSON_DATABASE_PATH));
        final Reader jsonReader = mock(ReaderJsonImpl.class);
        when(jsonReader.getFileExtension()).thenCallRealMethod();

        final String expected = String.valueOf(TestUtils.JSON_TABLE_PATH);
        final String actual = FileNameCreator.createTableFileName("myTable", jsonReader, database);
        assertEquals(expected, actual);
    }

    @Test
    void testCreateTableFileName_xml() {
        when(database.getURL()).thenReturn(String.valueOf(TestUtils.XML_DATABASE_PATH));
        final Reader xmlReader = mock(ReaderXmlImpl.class);
        when(xmlReader.getFileExtension()).thenCallRealMethod();

        final String expected = String.valueOf(TestUtils.XML_TABLE_PATH);
        final String actual = FileNameCreator.createTableFileName("myTable", xmlReader, database);
        assertEquals(expected, actual);
    }

    @Test
    void testCreateSchemaFileName_json() {
        when(database.getURL()).thenReturn(String.valueOf(TestUtils.JSON_DATABASE_PATH));
        final Reader jsonReader = mock(ReaderJsonImpl.class);
        when(jsonReader.getFileExtension()).thenCallRealMethod();
        when(jsonReader.getSchemaFileExtension()).thenCallRealMethod();

        final String expected = String.valueOf(TestUtils.JSON_SCHEMA_PATH);
        final String actual = FileNameCreator.createSchemaFileName("myTable", jsonReader, database);
        assertEquals(expected, actual);
    }

    @Test
    void testCreateSchemaFileName_xml() {
        when(database.getURL()).thenReturn(String.valueOf(TestUtils.XML_DATABASE_PATH));
        final Reader xmlReader = mock(ReaderXmlImpl.class);
        when(xmlReader.getSchemaFileExtension()).thenCallRealMethod();

        final String expected = String.valueOf(TestUtils.XSD_PATH);
        final String actual = FileNameCreator.createSchemaFileName("myTable", xmlReader, database);
        assertEquals(expected, actual);
    }

    @Test
    void testCreateDatabaseFileName_json() {
        final Reader jsonReader = mock(ReaderJsonImpl.class);
        when(jsonReader.getFileExtension()).thenCallRealMethod();
        final String expected = String.valueOf(
            Path.of(TestUtils.DATABASE_URL, Path.of(TestUtils.DATABASE_PATH).getFileName() + ".json"));
        final String actual = FileNameCreator.createDatabaseFileName(TestUtils.DATABASE_PATH, jsonReader);
        assertEquals(expected, actual);
    }

    @Test
    void testCreateDatabaseFileName_xml() {
        final Reader xmlReader = mock(ReaderXmlImpl.class);
        when(xmlReader.getFileExtension()).thenCallRealMethod();
        final String expected = String.valueOf(
            Path.of(TestUtils.DATABASE_URL, Path.of(TestUtils.DATABASE_PATH).getFileName() + ".xml"));
        final String actual = FileNameCreator.createDatabaseFileName(TestUtils.DATABASE_PATH, xmlReader);
        assertEquals(expected, actual);
    }
}
