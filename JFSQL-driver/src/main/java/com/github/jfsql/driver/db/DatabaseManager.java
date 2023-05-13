package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.validation.SemanticValidator;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.LinkedList;
import lombok.Getter;

public abstract class DatabaseManager {

    final Reader reader;
    final Writer writer;
    private final SemanticValidator semanticValidator;
    @Getter
    protected Database database;
    @Getter
    private final String URL;

    protected DatabaseManager(final String connectionString, final SemanticValidator semanticValidator,
        final Reader reader, final Writer writer) throws SQLException {
        this.semanticValidator = semanticValidator;
        this.reader = reader;
        this.writer = writer;
        URL = removePrefixFromUrl(connectionString);
        final Path databaseFileURL = FileNameCreator.createDatabaseFileName(URL, reader);
        final String databaseFileName = String.valueOf(databaseFileURL.getFileName());
        database = new Database(databaseFileName, databaseFileURL, new LinkedList<>());
        if (Files.exists(databaseFileURL)) {
            openDatabase();
        } else {
            initDatabase(database);
        }
    }

    private String removePrefixFromUrl(final String connectionString) throws SQLException {
        String modifiedURL = connectionString.replace("jdbc:jfsql:", "");
        if (!modifiedURL.endsWith(File.separator)) {
            modifiedURL += File.separator;
        }
        if (semanticValidator.URLIsNotDirectory(modifiedURL)) {
            throw new SQLException("URL is not a directory.");
        }
        return modifiedURL;
    }

    public abstract void initDatabase(final Database database) throws SQLException;

    public abstract void openDatabase() throws SQLException;

}
