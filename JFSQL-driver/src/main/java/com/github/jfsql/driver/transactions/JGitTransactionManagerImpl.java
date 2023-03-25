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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
                writeUncommittedObjects();

                final Map<File, Boolean> filesToAdd = getFilesToAdd();
                for (final Map.Entry<File, Boolean> entry : filesToAdd.entrySet()) {
                    final File file = entry.getKey();
                    if (Boolean.TRUE.equals(entry.getValue())) {
                        git.add().addFilepattern(file.getName()).call();
                    } else {
                        git.rm().addFilepattern(file.getName());
                        Files.delete(file.toPath());
                    }
                }

                // no args, the commit() was called explicitly through the connection object
                if (args.length == 0) {
                    git.commit().setMessage("Explicit commit").call();
                } else {
                    git.commit().setMessage("Auto committing: " + Arrays.toString(args)).call();
                }
            } catch (final GitAPIException | IOException e) {
                throw new SQLException(e);
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

    /**
     * The method searches for files in the database directory, and returns a map with each file as a key and a boolean
     * value indicating whether the file should be added in the next commit, or removed. The value for the database file
     * itself is set to true.
     */
    private Map<File, Boolean> getFilesToAdd() throws IOException {
        final Database database = databaseManager.database;
        final Path databaseUrl = database.getUrl();
        final String fileExtension = reader.getFileExtension();
        final String schemaExtension = reader.getSchemaFileExtension();
        final String[] extensions = new String[]{fileExtension, schemaExtension};
        final Collection<File> files = FileUtils.listFiles(databaseUrl.getParent().toFile(), extensions, false);
        final Map<File, Boolean> filesToAdd = new HashMap<>();
        for (final File file : files) {
            if (Objects.equals(Path.of(file.getAbsolutePath()), databaseUrl)) {
                filesToAdd.put(databaseUrl.toFile(), true);
                continue;
            }
            final boolean addFile = reader.pathIsPresentInDatabaseFile(database, file.getAbsolutePath());
            filesToAdd.put(file, addFile);
        }
        return filesToAdd;
    }

}
