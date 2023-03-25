package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.PessimisticLockException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import lombok.Data;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Data
public abstract class TransactionManager {

    private static final Logger logger = LogManager.getLogger(TransactionManager.class);
    static final Object lock = new Object();
    private static final Map<String, Long> FILE_TO_THREAD_ID_MAP = new HashMap<>();
    final Reader reader;
    final Writer writer;
    final Set<Table> uncommittedTables;
    final Set<Schema> uncommittedSchemas;
    final Set<Database> uncommittedDatabases;
    final DatabaseManager databaseManager;
    boolean autoCommit;

    protected TransactionManager(final DatabaseManager databaseManager, final Reader reader, final Writer writer) {
        autoCommit = true;
        this.databaseManager = databaseManager;
        this.reader = reader;
        this.writer = writer;
        uncommittedTables = new LinkedHashSet<>();
        uncommittedSchemas = new LinkedHashSet<>();
        uncommittedDatabases = new LinkedHashSet<>();
    }

    public abstract void commit(String... args) throws SQLException;

    public abstract void rollback() throws SQLException;

    public boolean getAutoCommit() {
        return autoCommit;
    }

    public void setAutoCommit(final boolean autoCommit) {
        this.autoCommit = autoCommit;
    }

    void writeUncommittedObjects() throws IOException {
        synchronized (lock) {
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
    }

    public void executeDMLOperation(final Table table) throws SQLException {
        synchronized (lock) {
            if (!autoCommit) {
                addTableToUncommittedObjects(table);
            } else {
                try {
                    writer.writeTable(table);
                    commit(String.valueOf(Path.of(table.getTableFile()).getFileName()));
                } catch (final IOException e) {
                    e.printStackTrace();
                    rollback();
                }
            }
        }
    }

    public void executeDDLOperation(final Table table, final Schema schema) throws SQLException {
        synchronized (lock) {
            final Database database = databaseManager.getDatabase();
            if (!autoCommit) {
                addDatabaseToUncommittedObjects(database);
                addSchemaToUncommittedObjects(schema);
                addTableToUncommittedObjects(table);
            } else {
                try {
                    writer.writeDatabaseFile(database);
                    writer.writeSchema(schema);
                    writer.writeTable(table);
                    commit(String.valueOf(database.getUrl().getFileName()),
                        String.valueOf(Path.of(schema.getSchemaFile()).getFileName()),
                        String.valueOf(Path.of(table.getTableFile()).getFileName()));
                } catch (final IOException e) {
                    e.printStackTrace();
                    rollback();
                }
            }
        }
    }

    public void executeDropTableOperation() throws SQLException {
        synchronized (lock) {
            final Database database = databaseManager.getDatabase();
            if (!autoCommit) {
                addDatabaseToUncommittedObjects(database);
            } else {
                try {
                    writer.writeDatabaseFile(database);
                    commit(String.valueOf(database.getUrl().getFileName()));
                } catch (final IOException e) {
                    e.printStackTrace();
                    rollback();
                }
            }
        }
    }

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        databaseManager.initDatabase(database);
    }

    private void addTableToUncommittedObjects(final Table table) {
        if (FILE_TO_THREAD_ID_MAP.containsKey(table.getTableFile())) {
            if (!Objects.equals(FILE_TO_THREAD_ID_MAP.get(table.getTableFile()), Thread.currentThread().getId())) {
                // remove all entries from the shared map, where the value was the thread's id
                removeCurrentThreadChangesFromMap();
                throw new PessimisticLockException(
                    "The file '" + table.getTableFile() + "' is currently modified by another thread.");
            }
        } else {
            FILE_TO_THREAD_ID_MAP.put(table.getTableFile(), Thread.currentThread().getId());
        }
        uncommittedTables.add(table);
    }

    private void addSchemaToUncommittedObjects(final Schema schema) {
        if (FILE_TO_THREAD_ID_MAP.containsKey(schema.getSchemaFile())) {
            if (!Objects.equals(FILE_TO_THREAD_ID_MAP.get(schema.getSchemaFile()),
                Thread.currentThread().getId())) {
                // remove all entries from the shared map, where the value was the thread's id
                removeCurrentThreadChangesFromMap();
                throw new PessimisticLockException(
                    "The file '" + schema.getSchemaFile() + "' is currently modified by another thread.");
            }
        } else {
            FILE_TO_THREAD_ID_MAP.put(schema.getSchemaFile(), Thread.currentThread().getId());
        }
        uncommittedSchemas.add(schema);
    }

    private void addDatabaseToUncommittedObjects(final Database database) {
        if (FILE_TO_THREAD_ID_MAP.containsKey(String.valueOf(database.getUrl()))) {
            if (!Objects.equals(FILE_TO_THREAD_ID_MAP.get(String.valueOf(database.getUrl())),
                Thread.currentThread().getId())) {
                // remove all entries from the shared map, where the value was the thread's id
                removeCurrentThreadChangesFromMap();
                throw new PessimisticLockException(
                    "The file '" + database.getUrl() + "' is currently modified by another thread.");
            }
        } else {
            FILE_TO_THREAD_ID_MAP.put(String.valueOf(database.getUrl()), Thread.currentThread().getId());
        }
        uncommittedDatabases.add(database);
    }

    void removeCurrentThreadChangesFromMap() {
        synchronized (FILE_TO_THREAD_ID_MAP) {
            final Iterator<Map.Entry<String, Long>> iterator = FILE_TO_THREAD_ID_MAP.entrySet().iterator();
            while (iterator.hasNext()) {
                final Map.Entry<String, Long> entry = iterator.next();
                final Long value = entry.getValue();
                if (Objects.equals(value, Thread.currentThread().getId())) {
                    iterator.remove();
                }
            }
        }
    }

}
