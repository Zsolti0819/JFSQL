package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.CommitFailedException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Data
public abstract class TransactionManager {

    private static final Logger logger = LogManager.getLogger(TransactionManager.class);
    final ThreadLocal<Set<Table>> uncommittedTables = ThreadLocal.withInitial(HashSet::new);
    final ThreadLocal<Set<Table>> uncommittedSchemas = ThreadLocal.withInitial(HashSet::new);
    final ThreadLocal<Set<Database>> uncommittedDatabases = ThreadLocal.withInitial(HashSet::new);
    final Reader reader;
    final Writer writer;
    final DatabaseManager databaseManager;
    boolean autoCommit;

    protected TransactionManager(final DatabaseManager databaseManager, final Reader reader, final Writer writer) {
        autoCommit = true;
        this.databaseManager = databaseManager;
        this.reader = reader;
        this.writer = writer;
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
        for (final Database db : uncommittedDatabases.get()) {
            writer.writeDatabaseFile(db);
        }
        for (final Table table : uncommittedSchemas.get()) {
            writer.writeSchema(table);
        }
        for (final Table table : uncommittedTables.get()) {
            writer.writeTable(table);
        }
        uncommittedDatabases.get().clear();
        uncommittedSchemas.get().clear();
        uncommittedTables.get().clear();
    }

    /**
     * Called when executing DROP TABLE -> then we only edit the database file
     */
    public void executeOperation(final Database database) throws SQLException {
        if (!autoCommit) {
            uncommittedDatabases.get().add(database);
        } else {
            try {
                writer.writeDatabaseFile(database);
                commit(String.valueOf(database.getURL().getFileName()));
            } catch (final IOException | CommitFailedException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    /**
     * Called when executing INSERT, UPDATE, DELETE or ALTERING table (ADD/DROP/RENAME COLUMN) -> then we edit the table
     * and may edit the schema too
     */
    public void executeOperation(final Table table, final boolean writeSchema) throws SQLException {
        if (!autoCommit) {
            if (writeSchema) {
                uncommittedSchemas.get().add(table);
            }
            uncommittedTables.get().add(table);
        } else {
            try {
                if (writeSchema) {
                    writer.writeSchema(table);
                    writer.writeTable(table);
                    commit(String.valueOf(Path.of(table.getSchemaFile()).getFileName()),
                        String.valueOf(Path.of(table.getTableFile()).getFileName()));
                    return;
                }
                writer.writeTable(table);
                commit(String.valueOf(Path.of(table.getTableFile()).getFileName()));
            } catch (final IOException | CommitFailedException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    /**
     * Called when executing CREATE TABLE, ALTER TABLE (RENAME TABLE) -> then we edit the table, schema and database
     * file
     */
    public void executeOperation(final Database database, final Table table) throws SQLException {
        if (!autoCommit) {
            uncommittedDatabases.get().add(database);
            uncommittedSchemas.get().add(table);
            uncommittedTables.get().add(table);
        } else {
            try {
                writer.writeDatabaseFile(database);
                writer.writeSchema(table);
                writer.writeTable(table);
                commit(String.valueOf(database.getURL().getFileName()),
                    String.valueOf(Path.of(table.getSchemaFile()).getFileName()),
                    String.valueOf(Path.of(table.getTableFile()).getFileName()));
            } catch (final IOException | CommitFailedException e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    Map<File, Boolean> getFilesToKeep() throws IOException {
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
