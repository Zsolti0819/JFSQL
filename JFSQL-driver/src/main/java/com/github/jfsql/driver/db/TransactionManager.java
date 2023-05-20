package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public abstract class TransactionManager {

    final Set<Database> uncommittedDatabases;
    final Set<Table> uncommittedSchemas;
    final Set<Table> uncommittedTables;
    final Map<String, Boolean> filesToKeep;
    final Reader reader;
    final Writer writer;
    final Database database;
    boolean autoCommit;

    protected TransactionManager(final Database database, final Reader reader, final Writer writer) {
        autoCommit = true;
        this.database = database;
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

    public void execute(final Table table, final Map<String, Boolean> blobsToKeep, final Operation operation)
        throws SQLException {
        if (!autoCommit) {
            executeAutoCommitFalse(table, blobsToKeep, operation);
        } else {
            try {
                executeAutoCommitTrue(table, blobsToKeep, operation);
            } catch (final Exception e) {
                e.printStackTrace();
                rollback();
            }
        }
    }

    private void executeAutoCommitTrue(final Table table, final Map<String, Boolean> blobsToKeep,
        final Operation operation)
        throws IOException, SQLException {
        switch (operation) {
            case DROP_TABLE:
                writer.writeDatabaseFile(database);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getTableFile(), false);
                filesToKeep.put(table.getSchemaFile(), false);
                filesToKeep.putAll(blobsToKeep);
                break;
            case INSERT:
            case DELETE:
            case UPDATE:
                writer.writeTable(table);
                filesToKeep.put(table.getTableFile(), true);
                filesToKeep.putAll(blobsToKeep);
                break;
            case ALTER_TABLE_ADD_COLUMN:
            case ALTER_TABLE_DROP_COLUMN:
            case ALTER_TABLE_RENAME_COLUMN:
                writer.writeSchema(table);
                writer.writeTable(table);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                filesToKeep.putAll(blobsToKeep);
                break;
            case ALTER_TABLE_RENAME_TABLE:
            case CREATE_TABLE:
                writer.writeDatabaseFile(database);
                writer.writeSchema(table);
                writer.writeTable(table);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                filesToKeep.putAll(blobsToKeep);
                break;
        }
        commit();
    }

    private void executeAutoCommitFalse(final Table table, final Map<String, Boolean> blobsToKeep,
        final Operation operation) {
        switch (operation) {
            case DROP_TABLE:
                uncommittedDatabases.add(database);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getTableFile(), false);
                filesToKeep.put(table.getSchemaFile(), false);
                filesToKeep.putAll(blobsToKeep);
                break;
            case INSERT:
            case DELETE:
            case UPDATE:
                uncommittedTables.add(table);
                filesToKeep.put(table.getTableFile(), true);
                filesToKeep.putAll(blobsToKeep);
                break;
            case ALTER_TABLE_ADD_COLUMN:
            case ALTER_TABLE_DROP_COLUMN:
            case ALTER_TABLE_RENAME_COLUMN:
                uncommittedSchemas.add(table);
                uncommittedTables.add(table);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                filesToKeep.putAll(blobsToKeep);
                break;
            case ALTER_TABLE_RENAME_TABLE:
            case CREATE_TABLE:
                uncommittedDatabases.add(database);
                uncommittedSchemas.add(table);
                uncommittedTables.add(table);
                filesToKeep.put(String.valueOf(database.getURL()), true);
                filesToKeep.put(table.getSchemaFile(), true);
                filesToKeep.put(table.getTableFile(), true);
                filesToKeep.putAll(blobsToKeep);
                break;
        }
    }

}
