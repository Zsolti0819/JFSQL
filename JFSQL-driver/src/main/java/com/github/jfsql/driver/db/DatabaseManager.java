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
import lombok.Data;

@Data
public abstract class DatabaseManager {

    private final String URL;
    private final SemanticValidator semanticValidator;
    private final FileNameCreator fileNameCreator;
    final Reader reader;
    final Writer writer;
    Database database;

    protected DatabaseManager(final String URL, final SemanticValidator semanticValidator,
        final FileNameCreator fileNameCreator, final Reader reader, final Writer writer) throws SQLException {
        this.semanticValidator = semanticValidator;
        this.fileNameCreator = fileNameCreator;
        this.reader = reader;
        this.writer = writer;
        String modifiedURL = URL.replace("jdbc:jfsql:", "");
        if (!modifiedURL.endsWith(File.separator)) {
            modifiedURL += File.separator;
        }
        if (semanticValidator.URLIsNotDirectory(modifiedURL)) {
            throw new SQLException("URL is not a directory.");
        }
        final Path databaseFileName = fileNameCreator.createDatabaseFileName(modifiedURL);
        database = new Database(databaseFileName, new LinkedList<>());
        if (Files.exists(databaseFileName)) {
            openDatabase();
        } else {
            initDatabase(database);
        }
        this.URL = modifiedURL;
    }

    public abstract void initDatabase(final Database database) throws SQLException;

    public abstract void openDatabase() throws SQLException;

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        initDatabase(database);
    }

}
