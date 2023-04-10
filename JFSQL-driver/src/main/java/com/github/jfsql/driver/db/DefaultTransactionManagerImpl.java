package com.github.jfsql.driver.db;

import com.github.jfsql.driver.exceptions.CommitFailedException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DefaultTransactionManagerImpl extends TransactionManager {

    private static final Logger logger = LogManager.getLogger(DefaultTransactionManagerImpl.class);

    public DefaultTransactionManagerImpl(final DatabaseManager databaseManager, final Reader reader,
        final Writer writer) {
        super(databaseManager, reader, writer);
    }

    @Override
    public void commit(final String... args) {
        try {
            writeUncommittedObjects();

            final Map<File, Boolean> filesToKeep = getFilesToKeep();
            logger.trace("filesToKeep = {}", filesToKeep);

            for (final Map.Entry<File, Boolean> entry : filesToKeep.entrySet()) {
                final File file = entry.getKey();
                if (Boolean.FALSE.equals(entry.getValue())) {
                    Files.delete(Path.of(file.getAbsolutePath()));
                }
            }
        } catch (final IOException e) {
            throw new CommitFailedException("Commit failed.\n" + e.getMessage());
        }
        removeCurrentThreadChangesFromMap();
    }

    @Override
    public void rollback() {
        // NO-OP
    }

}
