package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import lombok.Data;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Data
public abstract class TransactionManager {

    protected final Reader reader;
    protected final Writer writer;
    protected Database database;
    boolean autoCommit;
    private final List<Table> uncommittedTables;
    private final List<Table> uncommittedSchemas;
    private final List<Database> uncommittedDatabases;

    protected TransactionManager(final Path url, final Reader reader, final Writer writer) throws SQLException {
        autoCommit = true;
        this.reader = reader;
        this.writer = writer;
        final Path databaseFile = Path.of(url + File.separator + url.getFileName() + "." + reader.getFileExtension());
        database = new Database(databaseFile);
        if (!database.getUrl().toFile().exists()) {
            initDatabase(database);
        } else {
            openDatabase();
        }
        uncommittedTables = new ArrayList<>();
        uncommittedSchemas = new ArrayList<>();
        uncommittedDatabases = new ArrayList<>();
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
            for (final Database db : uncommittedDatabases) {
                writer.writeDatabaseFile(db);
            }
            for (final Table table : uncommittedSchemas) {
                writer.writeSchema(table);
            }
            for (final Table table : uncommittedTables) {
                writer.writeTable(table);
            }
        } catch (final SQLException e) {
            e.printStackTrace();
            rollback();
        } finally {
            uncommittedDatabases.clear();
            uncommittedSchemas.clear();
            uncommittedTables.clear();
        }
    }

    public void executeDMLOperation(final Table table) throws SQLException {
        if (!autoCommit) {
            addTableToUncommittedObjects(table);
        } else {
            try {
                writer.writeTable(table);
                commit();
            } catch (final SQLException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    public void executeDDLOperation(final Table table) throws SQLException {
        if (!autoCommit) {
            addSchemaToUncommittedObjects(table);
            addTableToUncommittedObjects(table);
            addDatabaseToUncommittedObjects(database);
        } else {
            try {
                writer.writeSchema(table);
                writer.writeTable(table);
                writer.writeDatabaseFile(database);
                commit();
            } catch (final SQLException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    public void executeDropTableOperation() throws SQLException {
        if (!autoCommit) {
            addDatabaseToUncommittedObjects(database);
        } else {
            try {
                writer.writeDatabaseFile(database);
                commit();
            } catch (final SQLException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        initDatabase(database);
    }

    public void addTableToUncommittedObjects(final Table table) {
        uncommittedTables.removeIf(t -> Objects.equals(t.getName(), table.getName()));
        uncommittedTables.add(table);
    }

    public void addSchemaToUncommittedObjects(final Table table) {
        uncommittedSchemas.removeIf(t -> Objects.equals(t.getName(), table.getName()));
        uncommittedSchemas.add(table);
    }

    public void addDatabaseToUncommittedObjects(final Database database) {
        uncommittedDatabases.removeIf(db -> Objects.equals(db.getUrl(), database.getUrl()));
        uncommittedDatabases.add(database);
    }

}
