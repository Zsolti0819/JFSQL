package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import java.nio.file.Path;
import lombok.experimental.UtilityClass;

@UtilityClass
public class FileNameCreator {

    public String createTableFileName(final String tableName, final Reader reader, final Database database) {
        final String parentDirectory = String.valueOf(Path.of(database.getURL()).getParent());
        final Path tableFileName = Path.of(parentDirectory, tableName + "." + reader.getFileExtension());
        return String.valueOf(tableFileName);
    }

    public String createSchemaFileName(final String tableName, final Reader reader, final Database database) {
        final String parentDirectory = String.valueOf(Path.of(database.getURL()).getParent());
        final String schemaName =
            reader instanceof ReaderJsonImpl ?
                tableName + "Schema." + reader.getSchemaFileExtension() :
                tableName + "." + reader.getSchemaFileExtension();
        final Path schemaFileName = Path.of(parentDirectory, schemaName);
        return String.valueOf(schemaFileName);
    }

    public String createDatabaseFileName(final String URL, final Reader reader) {
        final Path pathURL = Path.of(URL);
        final String fileName = pathURL.getFileName() + "." + reader.getFileExtension();
        return String.valueOf(Path.of(URL, fileName));
    }

}