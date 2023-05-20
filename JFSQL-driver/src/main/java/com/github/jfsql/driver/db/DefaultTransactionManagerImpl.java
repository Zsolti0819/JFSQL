package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.CommitFailedException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DefaultTransactionManagerImpl extends TransactionManager {

    private static final Logger logger = LogManager.getLogger(DefaultTransactionManagerImpl.class);

    public DefaultTransactionManagerImpl(final Database database, final Reader reader,
        final Writer writer) {
        super(database, reader, writer);
    }

    @Override
    public void commit() {
        try {
            writeUncommittedObjects();
            logger.trace("filesToKeep = {}", filesToKeep);

            for (final Map.Entry<String, Boolean> entry : filesToKeep.entrySet()) {
                final File file = new File(entry.getKey());
                if (Boolean.FALSE.equals(entry.getValue())) {
                    Files.delete(Path.of(file.getAbsolutePath()));
                }
            }
        } catch (final IOException e) {
            throw new CommitFailedException(e);
        } finally {
            filesToKeep.clear();
            SharedMapHandler.removeCurrentThreadChangesFromMap();
        }
    }

    @Override
    public void rollback() throws SQLException {
        logger.warn("Executing rollback()");
        try {
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final IOException e) {
            throw new SQLException("There was an error executing rollback().\n" + e.getMessage());
        }
    }

}
