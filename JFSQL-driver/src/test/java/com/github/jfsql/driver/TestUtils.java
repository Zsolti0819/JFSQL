package com.github.jfsql.driver;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import lombok.experimental.UtilityClass;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@UtilityClass
public class TestUtils {

    private static final Logger logger = LogManager.getLogger(TestUtils.class);
    private static final String BASE_PATH = String.valueOf(Path.of("").toAbsolutePath());
    public static final String DATABASE_PATH = String.valueOf(
        Path.of(BASE_PATH, "src", "main", "resources", "myDatabase"));
    public static final String DATABASE_URL = String.valueOf(DATABASE_PATH);
    public static final String XML_DATABASE_PATH = String.valueOf(
        Path.of(DATABASE_URL, Path.of(DATABASE_PATH).getFileName() + ".xml"));
    public static final String JSON_DATABASE_PATH = String.valueOf(
        Path.of(DATABASE_URL, Path.of(DATABASE_PATH).getFileName() + ".json"));
    public static final String XML_TABLE_PATH = String.valueOf(Path.of(DATABASE_URL, "myTable.xml"));
    public static final String JSON_TABLE_PATH = String.valueOf(Path.of(DATABASE_URL, "myTable.json"));
    public static final String XSD_PATH = String.valueOf(Path.of(DATABASE_URL, "myTable.xsd"));
    public static final String JSON_SCHEMA_PATH = String.valueOf(Path.of(DATABASE_URL, "myTableSchema.json"));
    public static final String EDITED_XML_TABLE_XML_PATH = String.valueOf(Path.of(DATABASE_URL, "myTableEdited.xml"));
    public static final String EDITED_JSON_TABLE_PATH = String.valueOf(Path.of(DATABASE_URL, "myTableEdited.json"));
    public static final String EDITED_XSD_PATH = String.valueOf(Path.of(DATABASE_URL, "myTableEdited.xsd"));
    public static final String EDITED_JSON_SCHEMA_PATH = String.valueOf(
        Path.of(DATABASE_URL, "myTableEditedSchema.json"));
    public static final String BLOB_COPY_PATH = String.valueOf(Path.of(DATABASE_URL, "java.sql.Driver"));
    public static final String ENCODED_JSON_BLOB_PATH = String.valueOf(Path.of(DATABASE_URL, "blob", "blob1.json"));
    public static final String ENCODED_XML_BLOB_PATH = String.valueOf(Path.of(DATABASE_URL, "blob", "blob1.xml"));
    public static final String META_INF_DRIVER_PATH = String.valueOf(
        Path.of(BASE_PATH, "src", "main", "resources", "META-INF",
            "services", "java.sql.Driver"));

    // For testing the reader
    public static final String TEST_JSON_DATABASE_PATH = String.valueOf(
        Path.of(BASE_PATH, "src", "test", "resources", "myDatabase_JSON"));
    public static final String TEST_JSON_DATABASE_FILE_PATH = String.valueOf(
        Path.of(TEST_JSON_DATABASE_PATH, "myDatabase_JSON.json"));
    public static final String TEST_JSON_TABLE_PATH = String.valueOf(Path.of(TEST_JSON_DATABASE_PATH, "myTable.json"));
    public static final String TEST_JSON_SCHEMA_PATH = String.valueOf(Path.of(TEST_JSON_DATABASE_PATH,
        "myTableSchema.json"));
    public static final String TEST_XML_DATABASE_PATH = String.valueOf(Path.of(BASE_PATH, "src", "test", "resources",
        "myDatabase_XML"));
    public static final String TEST_XML_DATABASE_FILE_PATH = String.valueOf(Path.of(String.valueOf(TEST_XML_DATABASE_PATH),
        "myDatabase_XML.xml"));
    public static final String TEST_XML_TABLE_PATH = String.valueOf(
        Path.of(String.valueOf(TEST_XML_DATABASE_PATH), "myTable.xml"));
    public static final String TEST_XSD_PATH = String.valueOf(
        Path.of(String.valueOf(TEST_XML_DATABASE_PATH), "myTable.xsd"));
    public static final String URL = "jdbc:jfsql:" + TestUtils.DATABASE_URL;

    public void deleteDatabaseDirectory() {
        final File folder = new File(TestUtils.DATABASE_PATH);
        boolean isDeleted = false;
        while (!isDeleted) {
            try {
                FileUtils.deleteDirectory(folder);
                isDeleted = true;
            } catch (final IOException e) {
                logger.error("Folder is in use, will try again in 1s.");
                try {
                    Thread.sleep(1000);
                } catch (final InterruptedException ex) {
                    logger.warn(ex.getMessage());
                }
            }
        }
    }
}
