package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import org.apache.commons.io.FileUtils;
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
    public void commit(final String... args) throws SQLException {
        synchronized (lock) {
            final Database database = databaseManager.database;
            final File databaseDirectoryPath = database.getUrl().getParent().toFile();
            try (final Git git = Git.open(databaseDirectoryPath)) {
                deleteGitIndexFile();
                writeUncommittedObjects();
                final Collection<File> filesToDelete = getFilesThatShouldNotBePresent();
                for (final File file : filesToDelete) {
                    git.rm().addFilepattern(file.getName()).call();
                    Files.delete(file.toPath());
                }
                git.add().addFilepattern(".").call();
                // no args, the commit() was called explicitly through the connection object
                if (args.length == 0) {
                    git.commit().setMessage("Explicit commit").call();
                } else {
                    git.commit().setMessage("Auto committing: " + Arrays.toString(args)).call();
                }
            } catch (final GitAPIException | IOException e) {
                throw new SQLException("commit failed.\n " + e.getMessage());
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new SQLException("Failed to delete the index file.\n" + e.getMessage());
            } finally {
                removeCurrentThreadChangesFromMap();
            }
        }
    }

    @Override
    public void rollback() throws SQLException {
        logger.warn("transaction rollback...");
        final Database database = databaseManager.database;
        try (final Git git = Git.open(database.getUrl().getParent().toFile())) {
            final ResetCommand resetCommand = git.reset().setMode(ResetCommand.ResetType.HARD);
            resetCommand.call();
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final GitAPIException | IOException e) {
            throw new SQLException("There was an error executing the rollback.\n" + e.getMessage());
        }
    }

    private void deleteGitIndexFile() throws InterruptedException, IOException {
        final Database database = databaseManager.database;
        final Path databaseDirectory = database.getUrl().getParent();
        final Path lockFile = Path.of(databaseDirectory + File.separator + ".git" + File.separator + "index");

        if (!Files.exists(lockFile)) {
            return; // Lock file doesn't exist, nothing to do
        }

        int attempts = 10;
        boolean deleted = false;
        while (!deleted && attempts > 0) {
            deleted = Files.deleteIfExists(lockFile);
            if (!deleted) {
                // If the lock file couldn't be deleted, wait and try again
                Thread.sleep(500);
                attempts--;
            }
        }

        if (!deleted) {
            throw new IOException("Failed to delete lock file: " + lockFile);
        }
    }

    private Collection<File> getFilesThatShouldNotBePresent() throws IOException {
        final Database database = databaseManager.database;
        final Path databaseUrl = database.getUrl();
        final String fileExtension = reader.getFileExtension();
        final String schemaExtension = reader.getSchemaFileExtension();
        final String[] extensions = new String[]{fileExtension, schemaExtension};
        final Collection<File> files = FileUtils.listFiles(databaseUrl.getParent().toFile(), extensions, false);
        final Collection<File> filesToDelete = new ArrayList<>();
        files.removeIf(file -> Objects.equals(Path.of(file.getAbsolutePath()), databaseUrl));
        for (final File file : files) {
            if (!reader.pathIsPresentInDatabaseFile(database, file.getAbsolutePath())) {
                filesToDelete.add(file);
            }
        }
        return filesToDelete;
    }

}
