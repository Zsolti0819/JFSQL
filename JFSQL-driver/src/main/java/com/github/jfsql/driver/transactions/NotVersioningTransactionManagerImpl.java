package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class NotVersioningTransactionManagerImpl extends TransactionManager {


    public NotVersioningTransactionManagerImpl(final Path url, final Reader reader, final Writer writer)
        throws SQLException {
        super(url, reader, writer);
    }

    @Override
    public void commit() throws SQLException {
        writeUncommittedObjects();
    }

    @Override
    public void rollback() {
        throw new UnsupportedOperationException("Rollback is not possible in the not committing mode.");
    }

    @Override
    public void openDatabase() throws SQLException {
        final List<Table> tables = reader.readTablesFromDatabaseFile(database);
        database.setTables(tables);
    }

    @Override
    public void initDatabase(final Database database) throws SQLException {
        try {
            Files.createDirectories(database.getUrl().getParent());
        } catch (final IOException e) {
            throw new SQLException("Failed to create directory for the database.");
        }
        final List<Table> tables = new ArrayList<>();
        database.setTables(tables);
        writer.writeDatabaseFile(database);
    }
}
