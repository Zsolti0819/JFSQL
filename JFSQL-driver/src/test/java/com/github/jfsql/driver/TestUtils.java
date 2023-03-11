package com.github.jfsql.driver;

import lombok.experimental.UtilityClass;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

@UtilityClass
public class TestUtils {

    private static final Logger logger = LogManager.getLogger(TestUtils.class);
    private static final String BASE_PATH = System.getProperty("user.dir");
    public static final Path DATABASE_PATH = Path.of(
            BASE_PATH + File.separator + "src" + File.separator + "main" + File.separator + "resources" + File.separator
                    + "myDatabase" + File.separator);
    public static final Path DATABASE_XML_FILE_PATH = Path.of(
            DATABASE_PATH + File.separator + DATABASE_PATH.getFileName() + ".xml");
    public static final Path DATABASE_JSON_FILE_PATH = Path.of(
            DATABASE_PATH + File.separator + DATABASE_PATH.getFileName() + ".json");
    public static final Path TABLE_XML_FILE_PATH = Path.of(DATABASE_PATH + File.separator + "myTable.xml");
    public static final Path TABLE_JSON_FILE_PATH = Path.of(DATABASE_PATH + File.separator + "myTable.json");
    public static final Path TABLE_XSD_FILE_PATH = Path.of(DATABASE_PATH + File.separator + "myTable.xsd");
    public static final Path TABLE_JSON_SCHEMA_FILE_PATH = Path.of(
            DATABASE_PATH + File.separator + "myTableSchema.json");
    public static final Path EDITED_TABLE_XML_FILE_PATH = Path.of(DATABASE_PATH + File.separator + "myTableEdited.xml");
    public static final Path EDITED_TABLE_JSON_FILE_PATH = Path.of(
            DATABASE_PATH + File.separator + "myTableEdited.json");
    public static final Path EDITED_TABLE_XSD_FILE_PATH = Path.of(DATABASE_PATH + File.separator + "myTableEdited.xsd");
    public static final Path EDITED_TABLE_JSON_SCHEMA_FILE_PATH = Path.of(
            DATABASE_PATH + File.separator + "myTableEditedSchema.json");
    public static final Path DATABASE2_PATH = Path.of(
            BASE_PATH + File.separator + "src" + File.separator + "main" + File.separator + "resources" + File.separator
                    + "myDatabase2" + File.separator);
    public static final Path DATABASE2_XML_FILE_PATH = Path.of(
            DATABASE2_PATH + File.separator + DATABASE2_PATH.getFileName() + ".xml");
    public static final Path DATABASE2_JSON_FILE_PATH = Path.of(
            DATABASE2_PATH + File.separator + DATABASE2_PATH.getFileName() + ".json");
    public static final Path NOT_DIRECTORY_PATH = Path.of(BASE_PATH + File.separator + "pom.xml");
    public static final Path META_INF_DRIVER_FILE_PATH = Path.of(
            BASE_PATH + File.separator + "src" + File.separator + "main" + File.separator + "resources" + File.separator
                    + "META-INF" + File.separator + "services" + File.separator + "java.sql.Driver");
    public static final Path BLOB_COPY_FILE_PATH = Path.of(DATABASE_PATH + File.separator + "java.sql.Driver");
    public static final Path ENCODED_BLOB_PATH_JSON = Path.of(DATABASE_PATH + File.separator + "blob" + File.separator + "blob1.json");
    public static final Path ENCODED_BLOB_PATH_XML = Path.of(DATABASE_PATH + File.separator + "blob" + File.separator + "blob1.xml");


    public void deleteDatabaseDirectory() throws IOException {
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
