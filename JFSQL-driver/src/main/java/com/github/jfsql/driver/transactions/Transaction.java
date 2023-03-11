package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import lombok.experimental.SuperBuilder;

import java.sql.SQLException;

@SuperBuilder
public abstract class Transaction {

    protected final Database database;
    protected final Reader reader;
    protected final Writer writer;
    boolean autoCommit;

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
}
