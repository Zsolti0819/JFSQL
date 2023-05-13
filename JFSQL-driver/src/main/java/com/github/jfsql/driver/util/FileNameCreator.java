package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import java.io.File;
import java.nio.file.Path;
import lombok.experimental.UtilityClass;

@UtilityClass
public class FileNameCreator {

    public String createTableFileName(final String tableName, final Reader reader, final Database database) {
        final String parentDirectory = String.valueOf(database.getURL().getParent());
        return parentDirectory + File.separator + tableName + "." + reader.getFileExtension();
    }

    public String createSchemaFileName(final String tableName, final Reader reader, final Database database) {
        final String parentDirectory = String.valueOf(database.getURL().getParent());
        return reader instanceof ReaderJsonImpl ? parentDirectory + File.separator + tableName + "Schema."
            + reader.getSchemaFileExtension()
            : parentDirectory + File.separator + tableName + "." + reader.getSchemaFileExtension();
    }

    public Path createDatabaseFileName(final String URL, final Reader reader) {
        final Path pathURL = Path.of(URL);
        final String fileName = File.separator + pathURL.getFileName() + "." + reader.getFileExtension();
        return Path.of(pathURL + fileName);
    }

}
