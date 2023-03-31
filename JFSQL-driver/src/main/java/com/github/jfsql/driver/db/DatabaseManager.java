package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.LinkedList;
import lombok.Getter;
import lombok.Setter;

public abstract class DatabaseManager {

    @Getter
    @Setter
    Database database;
    Reader reader;
    Writer writer;

    protected DatabaseManager(final Path url, final Reader reader, final Writer writer) throws SQLException {
        this.reader = reader;
        this.writer = writer;
        final Path databaseFile = Path.of(url + File.separator + url.getFileName() + "." + reader.getFileExtension());
        database = new Database(databaseFile, new LinkedList<>());
        if (!database.getUrl().toFile().exists()) {
            initDatabase(database);
        } else {
            openDatabase();
        }
    }

    public abstract void initDatabase(final Database database) throws SQLException;

    public abstract void openDatabase() throws SQLException;

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        initDatabase(database);
    }

}
