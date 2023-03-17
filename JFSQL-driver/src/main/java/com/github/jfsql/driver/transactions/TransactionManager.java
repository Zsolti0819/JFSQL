package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import lombok.Data;

@Data
public abstract class TransactionManager {

    protected final Reader reader;
    protected final Writer writer;
    private final List<Table> uncommittedTables;
    private final List<Table> uncommittedSchemas;
    private final List<Database> uncommittedDatabases;
    protected Database database;
    boolean autoCommit;

    protected TransactionManager(final Path url, final Reader reader, final Writer writer) throws SQLException {
        autoCommit = true;
        this.reader = reader;
        this.writer = writer;
        final Path databaseFile = Path.of(url + File.separator + url.getFileName() + "." + reader.getFileExtension());
        database = new Database(databaseFile, new LinkedList<>());
        if (!database.getUrl().toFile().exists()) {
            initDatabase(database);
        } else {
            openDatabase();
        }
        uncommittedTables = new ArrayList<>();
        uncommittedSchemas = new ArrayList<>();
        uncommittedDatabases = new ArrayList<>();
    }

    public abstract void initDatabase(final Database database) throws SQLException;

    public abstract void openDatabase() throws SQLException;

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
