package com.github.jfsql.driver.db;

import static com.github.jfsql.driver.db.SharedMapHandler.OBJECT_NAME_TO_THREAD_ID_MAP;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.CommitFailedException;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.ResetCommand;
import org.eclipse.jgit.api.errors.GitAPIException;

public class JGitTransactionManagerImpl extends TransactionManager {

    private static final Logger logger = LogManager.getLogger(JGitTransactionManagerImpl.class);

    public JGitTransactionManagerImpl(final Database database, final Reader reader, final Writer writer) {
        super(database, reader, writer);
    }

    @Override
    public void commit() {
        synchronized (OBJECT_NAME_TO_THREAD_ID_MAP) {
            final File databaseDirectoryPath = Path.of(database.getURL()).getParent().toFile();
            try (final Git git = Git.open(databaseDirectoryPath)) {
                writeUncommittedObjects();

                logger.trace("filesToKeep = {}", filesToKeep);

                for (final Map.Entry<String, Boolean> entry : filesToKeep.entrySet()) {
                    final String key = entry.getKey();
                    if ("null".equals(key) || Objects.equals(null, key)) {
                        continue;
                    }
                    final File file = new File(key);
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

                final List<String> onlyFileNames = filesToKeep.keySet().stream()
                    .map(s -> String.valueOf(Path.of(s).getFileName())).collect(Collectors.toList());
                final String commitMessage = "Committing: " + onlyFileNames;
                git.commit().setMessage(commitMessage).call();

            } catch (final GitAPIException | IOException e) {
                throw new CommitFailedException(e);
            } finally {
                filesToKeep.clear();
                SharedMapHandler.removeCurrentThreadChangesFromMap();
            }
        }
    }

    @Override
    public void rollback() throws SQLException {
        logger.warn("Executing rollback()");
        try (final Git git = Git.open(Path.of(database.getURL()).getParent().toFile())) {
            final ResetCommand resetCommand = git.reset().setMode(ResetCommand.ResetType.HARD);
            resetCommand.call();
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final GitAPIException | IOException e) {
            throw new SQLException("There was an error executing rollback().\n" + e.getMessage());
        }
    }

}
