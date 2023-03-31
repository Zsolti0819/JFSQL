package com.github.jfsql.driver.db;

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
import java.util.Set;
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

    private Map<File, Boolean> getFilesToAdd() throws SQLException {
        final Map<File, Boolean> filesToAdd = new HashMap<>();
        final Database database = databaseManager.database;
        final Path databaseUrl = database.getUrl();
        final String fileExtension = reader.getFileExtension();
        final String schemaExtension = reader.getSchemaFileExtension();
        final String[] extensions = new String[]{fileExtension, schemaExtension};

        final Collection<File> allFiles = FileUtils.listFiles(databaseUrl.getParent().toFile(), extensions, false);
        final Set<File> filesInDatabaseFile = reader.getFilesInDatabaseFile(database);

        allFiles.removeAll(filesInDatabaseFile);
        allFiles.forEach(file -> filesToAdd.put(file, false));

        filesInDatabaseFile.forEach(file -> filesToAdd.put(file, true));
        filesToAdd.put(databaseUrl.toFile(), true);

        return filesToAdd;
    }

}