package com.github.jfsql.driver.db;

import static com.github.jfsql.driver.services.StatementServiceManager.lock;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.CommitFailedException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.ResetCommand;
import org.eclipse.jgit.api.errors.GitAPIException;

public class JGitTransactionManagerImpl extends TransactionManager {

    private static final Logger logger = LogManager.getLogger(JGitTransactionManagerImpl.class);

    public JGitTransactionManagerImpl(final DatabaseManager databaseManager, final Reader reader, final Writer writer) {
        super(databaseManager, reader, writer);
    }

    @Override
    public void commit(final String... args) {
        synchronized (lock) {
            final Database database = databaseManager.database;
            final File databaseDirectoryPath = database.getURL().getParent().toFile();
            try (final Git git = Git.open(databaseDirectoryPath)) {
                writeUncommittedObjects();

                final Map<File, Boolean> filesToKeep = getFilesToKeep();
                logger.trace("filesToKeep = {}", filesToKeep);

                for (final Map.Entry<File, Boolean> entry : filesToKeep.entrySet()) {
                    final File file = entry.getKey();
                    final String fileName = file.getName();
                    final String parentFolder = file.getParent();
                    final String prefix = parentFolder.endsWith("blob") ? "blob/" : "";
                    if (Boolean.TRUE.equals(entry.getValue())) {
                        git.add().addFilepattern(prefix + fileName).call();
                    } else {
                        // This removes the file even from the disk, IDK why
                        git.rm().addFilepattern(prefix + fileName).call();
                    }
                }

                final String commitMessage =
                    args.length == 0 ? "Explicit commit" : "Auto committing: " + Arrays.toString(args);
                git.commit().setMessage(commitMessage).call();

            } catch (final GitAPIException | IOException e) {
                throw new CommitFailedException("Commit failed.\n" + e.getMessage());
            } finally {
                removeCurrentThreadChangesFromMap();
            }
        }
    }

    @Override
    public void rollback() throws SQLException {
        synchronized (lock) {
            logger.warn("Executing rollback()");
            final Database database = databaseManager.database;
            try (final Git git = Git.open(database.getURL().getParent().toFile())) {
                final ResetCommand resetCommand = git.reset().setMode(ResetCommand.ResetType.HARD);
                resetCommand.call();
                final List<Table> tables = reader.readTablesFromDatabaseFile(database);
                database.setTables(tables);
            } catch (final GitAPIException | IOException e) {
                throw new SQLException("There was an error executing rollback().\n" + e.getMessage());
            }
        }
    }

}
