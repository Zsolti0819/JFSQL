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
    final Set<Database> uncommittedDatabases;
    final Set<Table> uncommittedSchemas;
    final Set<Table> uncommittedTables;
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
                commit(String.valueOf(database.getURL().getFileName()));
                break;
            case INSERT:
            case DELETE:
            case UPDATE:
                writer.writeTable(table);
                commit(String.valueOf(Path.of(table.getTableFile()).getFileName()));
                break;
            case ALTER_TABLE_ADD_COLUMN:
            case ALTER_TABLE_DROP_COLUMN:
            case ALTER_TABLE_RENAME_COLUMN:
                writer.writeSchema(table);
                writer.writeTable(table);
                commit(String.valueOf(Path.of(table.getSchemaFile()).getFileName()),
                    String.valueOf(Path.of(table.getTableFile()).getFileName()));
                break;
            case ALTER_TABLE_RENAME_TABLE:
            case CREATE_TABLE:
                writer.writeDatabaseFile(database);
                writer.writeSchema(table);
                writer.writeTable(table);
                commit(String.valueOf(database.getURL().getFileName()),
                    String.valueOf(Path.of(table.getSchemaFile()).getFileName()),
                    String.valueOf(Path.of(table.getTableFile()).getFileName()));
                break;
        }
    }

    private void executeAutoCommitFalse(final Table table, final Operation operation) {
        final Database database = databaseManager.getDatabase();
        switch (operation) {
            case DROP_TABLE:
                uncommittedDatabases.add(database);
                break;
            case INSERT:
            case DELETE:
            case UPDATE:
                uncommittedTables.add(table);
                break;
            case ALTER_TABLE_ADD_COLUMN:
            case ALTER_TABLE_DROP_COLUMN:
            case ALTER_TABLE_RENAME_COLUMN:
                uncommittedSchemas.add(table);
                uncommittedTables.add(table);
                break;
            case ALTER_TABLE_RENAME_TABLE:
            case CREATE_TABLE:
                uncommittedDatabases.add(database);
                uncommittedSchemas.add(table);
                uncommittedTables.add(table);
                break;
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
