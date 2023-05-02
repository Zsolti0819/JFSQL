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
import org.apache.commons.io.FileUtils;

public abstract class TransactionManager {

    final Set<Database> uncommittedDatabases;
    final Set<Table> uncommittedSchemas;
    final Set<Table> uncommittedTables;
    final Map<String, Boolean> filesToKeep;
    final Reader reader;
    final Writer writer;
    final DatabaseManager databaseManager;
    boolean autoCommit;

    protected TransactionManager(final DatabaseManager databaseManager, final Reader reader, final Writer writer) {
        autoCommit = true;
        this.databaseManager = databaseManager;
        this.reader = reader;
        this.writer = writer;
        uncommittedDatabases = new HashSet<>();
        uncommittedSchemas = new HashSet<>();
        uncommittedTables = new HashSet<>();
        filesToKeep = new HashMap<>();
    }

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

    public void execute(final Table table, final Operation operation) throws SQLException {
        try {
            if (!autoCommit) {
                executeAutoCommitFalse(table, operation);
            } else {
                executeAutoCommitTrue(table, operation);
            }
        } catch (final IOException | CommitFailedException e) {
            e.printStackTrace();
            rollback();
        }
    }

    private void executeAutoCommitTrue(final Table table, final Operation operation)
        throws IOException, SQLException {
        final Database database = databaseManager.getDatabase();
        switch (operation) {
            case DROP_TABLE:
                writer.writeDatabaseFile(database);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getTableFile(), false);
                filesToKeep.put(table.getSchemaFile(), false);
                commit();
                break;
            case INSERT:
            case DELETE:
            case UPDATE:
                writer.writeTable(table);
                filesToKeep.put(table.getTableFile(), true);
                commit();
                break;
            case ALTER_TABLE_ADD_COLUMN:
            case ALTER_TABLE_DROP_COLUMN:
            case ALTER_TABLE_RENAME_COLUMN:
                writer.writeSchema(table);
                writer.writeTable(table);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                commit();
                break;
            case ALTER_TABLE_RENAME_TABLE:
            case CREATE_TABLE:
                writer.writeDatabaseFile(database);
                writer.writeSchema(table);
                writer.writeTable(table);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                commit();
                break;
        }
    }

    private void executeAutoCommitFalse(final Table table, final Operation operation) {
        final Database database = databaseManager.getDatabase();
        switch (operation) {
            case DROP_TABLE:
                uncommittedDatabases.add(database);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getTableFile(), false);
                filesToKeep.put(table.getSchemaFile(), false);
                break;
            case INSERT:
            case DELETE:
            case UPDATE:
                uncommittedTables.add(table);
                filesToKeep.put(table.getTableFile(), true);
                break;
            case ALTER_TABLE_ADD_COLUMN:
            case ALTER_TABLE_DROP_COLUMN:
            case ALTER_TABLE_RENAME_COLUMN:
                uncommittedSchemas.add(table);
                uncommittedTables.add(table);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                break;
            case ALTER_TABLE_RENAME_TABLE:
            case CREATE_TABLE:
                uncommittedDatabases.add(database);
                uncommittedSchemas.add(table);
                uncommittedTables.add(table);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                break;
        }
    }

    Map<String, Boolean> getBlobsToKeep() throws IOException {
        final Map<String, Boolean> filesToKeep = new HashMap<>();
        final Database database = databaseManager.database;
        final Path databaseURL = database.getURL();
        final Path databaseFolder = databaseURL.getParent();
        final Path blobFolder = Path.of(databaseFolder + File.separator + "blob");
        final String fileExtension = reader.getFileExtension();
        final String schemaExtension = reader.getSchemaFileExtension();
        final String[] extensions = new String[]{fileExtension, schemaExtension};

        final Collection<File> blobFolderFiles = FileUtils.listFiles(blobFolder.toFile(), extensions, false);
        final Set<File> blobsFromTables = reader.getBlobsFromTables(database);

        for (final File file : blobFolderFiles) {
            final boolean isInTables = blobsFromTables.contains(file);
            filesToKeep.put(String.valueOf(file), isInTables);
        }
        return filesToKeep;
    }

}
