package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.PessimisticLockException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Data
public abstract class TransactionManager {

    static final Object lock = new Object();
    private static final Logger logger = LogManager.getLogger(TransactionManager.class);
    private static final Map<String, Long> FILE_TO_THREAD_ID_MAP = new HashMap<>();
    final Reader reader;
    final Writer writer;
    final Set<Table> uncommittedTables;
    final Set<Table> uncommittedSchemas;
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
            for (final Table table : uncommittedSchemas) {
                writer.writeSchema(table);
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

    public void executeDDLOperation(final Database database, final Table table)
        throws SQLException {
        synchronized (lock) {
            if (!autoCommit) {
                addDatabaseToUncommittedObjects(database);
                addSchemaToUncommittedObjects(table);
                addTableToUncommittedObjects(table);
            } else {
                try {
                    writer.writeDatabaseFile(database);
                    writer.writeSchema(table);
                    writer.writeTable(table);
                    commit(String.valueOf(database.getURL().getFileName()),
                        String.valueOf(Path.of(table.getSchemaFile()).getFileName()),
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
                    commit(String.valueOf(database.getURL().getFileName()));
                } catch (final IOException e) {
                    e.printStackTrace();
                    rollback();
                }
            }
        }
    }

    private void addTableToUncommittedObjects(final Table table) {
        if (FILE_TO_THREAD_ID_MAP.containsKey(table.getTableFile())) {
            if (!Objects.equals(FILE_TO_THREAD_ID_MAP.get(table.getTableFile()), Thread.currentThread().getId())) {
                // remove all entries from the shared map, where the value was the thread's id
                removeCurrentThreadChangesFromMap();
                throw new PessimisticLockException(
                    "The file '" + table.getTableFile() + "' is currently being modified by another thread.");
            }
        } else {
            FILE_TO_THREAD_ID_MAP.put(table.getTableFile(), Thread.currentThread().getId());
        }
        uncommittedTables.add(table);
    }

    private void addSchemaToUncommittedObjects(final Table table) {
        if (FILE_TO_THREAD_ID_MAP.containsKey(table.getSchemaFile())) {
            if (!Objects.equals(FILE_TO_THREAD_ID_MAP.get(table.getSchemaFile()),
                Thread.currentThread().getId())) {
                // remove all entries from the shared map, where the value was the thread's id
                removeCurrentThreadChangesFromMap();
                throw new PessimisticLockException(
                    "The file '" + table.getSchemaFile() + "' is currently being modified by another thread.");
            }
        } else {
            FILE_TO_THREAD_ID_MAP.put(table.getSchemaFile(), Thread.currentThread().getId());
        }
        uncommittedSchemas.add(table);
    }

    private void addDatabaseToUncommittedObjects(final Database database) {
        if (FILE_TO_THREAD_ID_MAP.containsKey(String.valueOf(database.getURL()))) {
            if (!Objects.equals(FILE_TO_THREAD_ID_MAP.get(String.valueOf(database.getURL())),
                Thread.currentThread().getId())) {
                // remove all entries from the shared map, where the value was the thread's id
                removeCurrentThreadChangesFromMap();
                throw new PessimisticLockException(
                    "The file '" + database.getURL() + "' is currently being modified by another thread.");
            }
        } else {
            FILE_TO_THREAD_ID_MAP.put(String.valueOf(database.getURL()), Thread.currentThread().getId());
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

    Map<File, Boolean> getFilesToKeep() throws SQLException {
        final Map<File, Boolean> filesToKeep = new HashMap<>();
        final Database database = databaseManager.database;
        final Path databaseURL = database.getURL();
        final Path databaseFolder = databaseURL.getParent();
        final Path blobFolder = Path.of(databaseFolder + File.separator + "blob");
        final String fileExtension = reader.getFileExtension();
        final String schemaExtension = reader.getSchemaFileExtension();
        final String[] extensions = new String[]{fileExtension, schemaExtension};

        final Collection<File> mainFolderFiles = FileUtils.listFiles(databaseFolder.toFile(), extensions, false);
        final Collection<File> blobFolderFiles = FileUtils.listFiles(blobFolder.toFile(), extensions, false);
        final Collection<File> allFiles = Stream.concat(mainFolderFiles.stream(), blobFolderFiles.stream())
            .collect(Collectors.toList());

        final Set<File> filesFromDatabaseFile = reader.getFilesFromDatabaseFile(database);
        final Set<File> blobsFromTables = reader.getBlobsFromTables(database);

        allFiles.removeIf(filesFromDatabaseFile::contains);
        allFiles.removeIf(blobsFromTables::contains);
        allFiles.forEach(file -> filesToKeep.put(file, false));

        Stream.concat(filesFromDatabaseFile.stream(), blobsFromTables.stream())
            .forEach(file -> filesToKeep.put(file, true));
        filesToKeep.put(databaseURL.toFile(), true);
        return filesToKeep;
    }

}
