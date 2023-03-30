package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import java.io.File;
import java.nio.file.Path;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class FileNameCreator {

    private final Reader reader;

    public String createTableFileName(final String tableName, final Database database) {
        final String parentDirectory = String.valueOf(database.getUrl().getParent());
        return parentDirectory + File.separator + tableName + "." + reader.getFileExtension();
    }

    public String createSchemaFileName(final String tableName, final Database database) {
        final String parentDirectory = String.valueOf(database.getUrl().getParent());
        return reader instanceof ReaderJsonImpl ? parentDirectory + File.separator + tableName + "Schema."
            + reader.getSchemaFileExtension()
            : parentDirectory + File.separator + tableName + "." + reader.getSchemaFileExtension();
    }

    public Path createDatabaseFileName(final CreateDatabaseWrapper statement) {
        final Path url = Path.of(statement.getDatabaseUrl());
        final String fileName = File.separator + url.getFileName() + "." + reader.getFileExtension();
        return Path.of(url + fileName);
    }

}
