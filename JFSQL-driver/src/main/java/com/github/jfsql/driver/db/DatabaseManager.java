package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.LinkedList;
import lombok.Data;

@Data
public abstract class DatabaseManager {

    private final String url;
    final Reader reader;
    final Writer writer;
    Database database;

    protected DatabaseManager(final String url, final Reader reader, final Writer writer) throws SQLException {
        this.reader = reader;
        this.writer = writer;
        String urlWithoutPrefix = url.replace("jdbc:jfsql:", "");
        if (!urlWithoutPrefix.endsWith(File.separator)) {
            urlWithoutPrefix += File.separator;
        }
        final Path databaseFileName = createDatabaseFileName(urlWithoutPrefix);
        database = new Database(databaseFileName, new LinkedList<>());
        if (Files.exists(databaseFileName)) {
            openDatabase();
        } else {
            initDatabase(database);
        }
        this.url = urlWithoutPrefix;
    }

    private Path createDatabaseFileName(final String urlWithoutPrefix) throws SQLException {
        final Path urlPath = Path.of(urlWithoutPrefix);
        if (Files.isRegularFile(urlPath)) {
            throw new SQLException("Url is not a directory.");
        }
        return Path.of(urlPath + File.separator + urlPath.getFileName() + "." + reader.getFileExtension());
    }

    public abstract void initDatabase(final Database database) throws SQLException;

    public abstract void openDatabase() throws SQLException;

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        initDatabase(database);
    }

}
