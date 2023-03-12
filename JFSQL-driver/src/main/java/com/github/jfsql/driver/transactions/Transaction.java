package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import lombok.Getter;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

@Getter
public abstract class Transaction {

    protected final Database database;
    protected final Reader reader;
    protected final Writer writer;
    boolean autoCommit;

    protected Transaction(final Path url, final Reader reader, final Writer writer) throws SQLException {
        autoCommit = true;
        this.reader = reader;
        this.writer = writer;
        final Path databaseFile = Path.of(url + File.separator + url.getFileName() + "." + writer.getFileExtension());
        database = new Database(databaseFile);
        if (!database.getUrl().toFile().exists()) {
            initDatabase(database);
        } else {
            openDatabase();
        }
    }

    public abstract void commit() throws SQLException;

    public abstract void rollback() throws SQLException;

    public boolean getAutoCommit() {
        return autoCommit;
    }

    public void setAutoCommit(final boolean autoCommit) {
        this.autoCommit = autoCommit;
    }

    void writeUncommittedObjects() throws SQLException {
        try {
            writer.writeUncommittedDatabases();
            writer.writeUncommittedSchemas();
            writer.writeUncommittedTables();
        } catch (final SQLException e) {
            e.printStackTrace();
            rollback();
        } finally {
            writer.getUncommittedDatabases().clear();
            writer.getUncommittedSchemas().clear();
            writer.getUncommittedTables().clear();
        }
    }

    public void openDatabase() throws SQLException {
        try (final Git ignored = Git.open(database.getUrl().getParent().toFile())) {
            final List<Table> tables = reader.readDatabaseFile(database);
            database.setTables(tables);
        } catch (final IOException e) {
            throw new SQLException("Couldn't open git repository.\n" + e.getMessage());
        }
    }

    public void initDatabase(final Database database) throws SQLException {
        try (final Git git = Git.init().setDirectory(database.getUrl().getParent().toFile()).call()) {
            final List<Table> tables = new ArrayList<>();
            database.setTables(tables);
            writer.writeDatabaseFile(database);
            git.add().addFilepattern(".").call();
            git.commit().setMessage("Initial commit").call();
        } catch (final GitAPIException e) {
            throw new SQLException("Couldn't init git repository.\n" + e.getMessage());
        }
    }

}
