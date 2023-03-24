package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.PessimisticLockException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import lombok.Data;

@Data
public abstract class TransactionManager {

    protected final Reader reader;
    protected final Writer writer;
    private static final Map<String, Long> FILE_THREADID_MAP = new HashMap<>();
    final Set<Table> uncommittedTables;
    final Set<Schema> uncommittedSchemas;
    final Set<Database> uncommittedDatabases;
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
        uncommittedTables = new LinkedHashSet<>();
        uncommittedSchemas = new LinkedHashSet<>();
        uncommittedDatabases = new LinkedHashSet<>();
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

    void writeUncommittedObjects() throws IOException {
        for (final Database db : uncommittedDatabases) {
            writer.writeDatabaseFile(db);
        }
        for (final Schema schema : uncommittedSchemas) {
            writer.writeSchema(schema);
        }
        for (final Table table : uncommittedTables) {
            writer.writeTable(table);
        }
        uncommittedDatabases.clear();
        uncommittedSchemas.clear();
        uncommittedTables.clear();
    }

    public void executeDMLOperation(final Table table) throws SQLException {
        if (!autoCommit) {
            try {
                addTableToUncommittedObjects(table);
            } catch (final PessimisticLockException e) {
                removeCurrentThreadChangesFromMap();
            }
        } else {
            try {
                writer.writeTable(table);
                commit();
            } catch (final IOException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    public void executeDDLOperation(final Table table, final Schema schema) throws SQLException {
        if (!autoCommit) {
            try {
                addDatabaseToUncommittedObjects(database);
                addSchemaToUncommittedObjects(schema);
                addTableToUncommittedObjects(table);
            } catch (final PessimisticLockException e) {
                removeCurrentThreadChangesFromMap();
            }
        } else {
            try {
                writer.writeDatabaseFile(database);
                writer.writeSchema(schema);
                writer.writeTable(table);
                commit();
            } catch (final IOException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    public void executeDropTableOperation() throws SQLException {
        if (!autoCommit) {
            try {
                addDatabaseToUncommittedObjects(database);
            } catch (final PessimisticLockException e) {
                removeCurrentThreadChangesFromMap();
            }
        } else {
            try {
                writer.writeDatabaseFile(database);
                commit();
            } catch (final IOException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        initDatabase(database);
    }

    public void addTableToUncommittedObjects(final Table table) {
        if (FILE_THREADID_MAP.containsKey(table.getTableFile())) {
            if (!Objects.equals(FILE_THREADID_MAP.get(table.getTableFile()), Thread.currentThread().getId())) {
                throw new PessimisticLockException("Pessimistic lock exception, the file '" + table.getTableFile()
                    + "' is currently modified by another thread.");
            }
        } else {
            FILE_THREADID_MAP.put(table.getTableFile(), Thread.currentThread().getId());
        }
        uncommittedTables.add(table);
    }

    public void addSchemaToUncommittedObjects(final Schema schema) {
        if (FILE_THREADID_MAP.containsKey(schema.getSchemaFile())) {
            if (!Objects.equals(FILE_THREADID_MAP.get(schema.getSchemaFile()),
                Thread.currentThread().getId())) {
                throw new PessimisticLockException("Pessimistic lock exception, the file '" + schema.getSchemaFile()
                    + "' is currently modified by another thread.");
            }
        } else {
            FILE_THREADID_MAP.put(schema.getSchemaFile(), Thread.currentThread().getId());
        }
        uncommittedSchemas.add(schema);
    }

    public void addDatabaseToUncommittedObjects(final Database database) {
        if (FILE_THREADID_MAP.containsKey(String.valueOf(database.getUrl()))) {
            if (!Objects.equals(FILE_THREADID_MAP.get(String.valueOf(database.getUrl())),
                Thread.currentThread().getId())) {
                throw new PessimisticLockException("Pessimistic lock exception, the file '" + database.getUrl()
                    + "' is currently modified by another thread.");
            }
        } else {
            FILE_THREADID_MAP.put(String.valueOf(database.getUrl()), Thread.currentThread().getId());
        }
        uncommittedDatabases.add(database);
    }

    private void removeCurrentThreadChangesFromMap() {
        for (final Map.Entry<String, Long> entry : FILE_THREADID_MAP.entrySet()) {
            final String key = entry.getKey();
            final Long value = entry.getValue();
            if (Objects.equals(value, Thread.currentThread().getId())) {
                FILE_THREADID_MAP.remove(key);
            }
        }
    }

}
