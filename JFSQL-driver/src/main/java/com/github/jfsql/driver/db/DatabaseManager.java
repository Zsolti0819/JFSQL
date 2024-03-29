package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.util.FileNameCreator;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.LinkedList;
import lombok.Getter;

public abstract class DatabaseManager {

    final Reader reader;
    final Writer writer;
    @Getter
    protected Database database;
    @Getter
    private final String URL;

    protected DatabaseManager(final String connectionString, final Reader reader, final Writer writer)
        throws SQLException {
        this.reader = reader;
        this.writer = writer;
        URL = removePrefixFromUrl(connectionString);
        final String databaseFileURL = FileNameCreator.createDatabaseFileName(URL, reader);
        final Path databaseFilePath = Path.of(databaseFileURL);
        final String databaseFileName = String.valueOf(databaseFilePath.getFileName());
        database = new Database(databaseFileName, databaseFileURL, new LinkedList<>());
        if (Files.exists(databaseFilePath)) {
            openDatabase();
        } else {
            initDatabase(database);
        }
    }

    private String removePrefixFromUrl(final String connectionString) throws SQLException {
        final String modifiedURL = connectionString.replace("jdbc:jfsql:", "");
        if (Path.of(modifiedURL).toFile().isFile()) {
            throw new SQLException("URL is not a directory.");
        }
        return modifiedURL;
    }

    public abstract void initDatabase(final Database database) throws SQLException;

    public abstract void openDatabase() throws SQLException;

}
