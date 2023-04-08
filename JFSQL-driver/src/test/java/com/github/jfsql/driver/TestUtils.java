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
    private static final String BASE_PATH = System.getProperty("user.dir");
    public static final Path DATABASE_PATH = Path.of(
        BASE_PATH + File.separator + "src" + File.separator + "main" + File.separator + "resources" + File.separator
            + "myDatabase" + File.separator);
    public static final Path XML_DATABASE_PATH = Path.of(
        DATABASE_PATH + File.separator + DATABASE_PATH.getFileName() + ".xml");
    public static final Path JSON_DATABASE_PATH = Path.of(
        DATABASE_PATH + File.separator + DATABASE_PATH.getFileName() + ".json");
    public static final Path XML_TABLE_PATH = Path.of(DATABASE_PATH + File.separator + "myTable.xml");
    public static final Path JSON_TABLE_PATH = Path.of(DATABASE_PATH + File.separator + "myTable.json");
    public static final Path XSD_PATH = Path.of(DATABASE_PATH + File.separator + "myTable.xsd");
    public static final Path JSON_SCHEMA_PATH = Path.of(DATABASE_PATH + File.separator + "myTableSchema.json");
    public static final Path EDITED_XML_TABLE_XML_PATH = Path.of(DATABASE_PATH + File.separator + "myTableEdited.xml");
    public static final Path EDITED_JSON_TABLE_PATH = Path.of(DATABASE_PATH + File.separator + "myTableEdited.json");
    public static final Path EDITED_XSD_PATH = Path.of(DATABASE_PATH + File.separator + "myTableEdited.xsd");
    public static final Path EDITED_JSON_SCHEMA_PATH = Path.of(
        DATABASE_PATH + File.separator + "myTableEditedSchema.json");
    public static final Path BLOB_COPY_PATH = Path.of(DATABASE_PATH + File.separator + "java.sql.Driver");
    public static final Path ENCODED_JSON_BLOB_PATH = Path.of(
        DATABASE_PATH + File.separator + "blob" + File.separator + "blob1.json");
    public static final Path ENCODED_XML_BLOB_PATH = Path.of(
        DATABASE_PATH + File.separator + "blob" + File.separator + "blob1.xml");
    public static final Path DATABASE2_PATH = Path.of(
        BASE_PATH + File.separator + "src" + File.separator + "main" + File.separator + "resources" + File.separator
            + "myDatabase2" + File.separator);
    public static final Path XML_DATABASE2_PATH = Path.of(
        DATABASE2_PATH + File.separator + DATABASE2_PATH.getFileName() + ".xml");
    public static final Path JSON_DATABASE2_PATH = Path.of(
        DATABASE2_PATH + File.separator + DATABASE2_PATH.getFileName() + ".json");
    public static final Path NOT_DIRECTORY_PATH = Path.of(BASE_PATH + File.separator + "pom.xml");
    public static final Path META_INF_DRIVER_PATH = Path.of(
        BASE_PATH + File.separator + "src" + File.separator + "main" + File.separator + "resources" + File.separator
            + "META-INF" + File.separator + "services" + File.separator + "java.sql.Driver");

    // For testing the reader
    public static final Path TEST_JSON_DATABASE_PATH = Path.of(
        BASE_PATH + File.separator + "src" + File.separator + "test" + File.separator + "resources" + File.separator
            + "myDatabase_JSON" + File.separator);
    public static final Path TEST_JSON_DATABASE_FILE_PATH = Path.of(
        TEST_JSON_DATABASE_PATH + File.separator + "myDatabase_JSON.json");
    public static final Path TEST_JSON_TABLE_PATH = Path.of(TEST_JSON_DATABASE_PATH + File.separator + "myTable.json");
    public static final Path TEST_JSON_SCHEMA_PATH = Path.of(
        TEST_JSON_DATABASE_PATH + File.separator + "myTableSchema.json");

    public static final Path TEST_XML_DATABASE_PATH = Path.of(
        BASE_PATH + File.separator + "src" + File.separator + "test" + File.separator + "resources" + File.separator
            + "myDatabase_XML" + File.separator);
    public static final Path TEST_XML_DATABASE_FILE_PATH = Path.of(
        TEST_XML_DATABASE_PATH + File.separator + "myDatabase_XML.xml");
    public static final Path TEST_XML_TABLE_PATH = Path.of(TEST_XML_DATABASE_PATH + File.separator + "myTable.xml");
    public static final Path TEST_XSD_PATH = Path.of(TEST_XML_DATABASE_PATH + File.separator + "myTable.xsd");

    public void deleteDatabaseDirectory() {
        final File folder = new File(TestUtils.DATABASE_PATH.toUri());
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
