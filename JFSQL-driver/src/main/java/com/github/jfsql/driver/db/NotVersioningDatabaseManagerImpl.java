package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class NotVersioningDatabaseManagerImpl extends DatabaseManager {

    public NotVersioningDatabaseManagerImpl(final Path url, final Reader reader, final Writer writer)
        throws SQLException {
        super(url, reader, writer);
    }

    @Override
    public void initDatabase(final Database database) throws SQLException {
        final Path databaseFolder = database.getUrl().getParent();
        final Path blobFolder = Path.of(databaseFolder + File.separator + "blob");
        try {
            Files.createDirectories(databaseFolder.getParent());
            Files.createDirectories(blobFolder.getParent());
        } catch (final IOException e) {
            throw new SQLException("Failed to create directory for the database.\n" + e.getMessage());
        }
        final List<Table> tables = new ArrayList<>();
        database.setTables(tables);
        try {
            writer.writeDatabaseFile(database);
        } catch (final IOException e) {
            throw new SQLException("Failed to write the database file.\n" + e.getMessage());
        }
    }

    @Override
    public void openDatabase() throws SQLException {
        final List<Table> tables = reader.readTablesFromDatabaseFile(database);
        database.setTables(tables);
    }
}
