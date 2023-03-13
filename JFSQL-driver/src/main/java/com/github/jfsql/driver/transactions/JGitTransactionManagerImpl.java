package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.ResetCommand;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

public class JGitTransactionManagerImpl extends TransactionManager {

    private static final Logger logger = LogManager.getLogger(JGitTransactionManagerImpl.class);

    public JGitTransactionManagerImpl(final Path url, final Reader reader, final Writer writer) throws SQLException {
        super(url, reader, writer);
    }

    @Override
    public void commit() throws SQLException {
        if (!autoCommit) {
            writeUncommittedObjects();
        }
        final File databaseDirectoryPath = database.getUrl().getParent().toFile();
        try (final Git git = Git.open(databaseDirectoryPath)) {
            final Collection<File> filesToDelete = getFilesThatShouldNotBePresent();
            for (final File file : filesToDelete) {
                git.rm().addFilepattern(file.getName()).call();
            }
            git.add().addFilepattern(".").call();
            git.commit().setMessage("Commit").call();
        } catch (final IOException | GitAPIException e) {
            e.printStackTrace();
            rollback();
        }
    }

    @Override
    public void rollback() throws SQLException {
        logger.warn("TransactionManager rollback...");
        try (final Git git = Git.open(database.getUrl().getParent().toFile())) {
            final ResetCommand resetCommand = git.reset().setMode(ResetCommand.ResetType.HARD);
            resetCommand.call();
            database.setTables(null);
        } catch (final GitAPIException | IOException e) {
            throw new SQLException("There was an error executing the rollback.\n" + e.getMessage());
        }
    }

    private Collection<File> getFilesThatShouldNotBePresent() throws SQLException {
        final String fileExtension = writer.getFileExtension();
        final String schemaExtension = writer.getSchemaFileExtension();
        final String[] extensions = new String[]{fileExtension, schemaExtension};
        final Collection<File> files = FileUtils.listFiles(database.getUrl().getParent().toFile(), extensions, false);
        final Collection<File> filesToDelete = new ArrayList<>();
        files.removeIf(file -> Objects.equals(Path.of(file.getAbsolutePath()), database.getUrl()));
        for (final File file : files) {
            if (!reader.pathIsPresentInDatabaseFile(database, file.getAbsolutePath())) {
                filesToDelete.add(file);
            }
        }
        return filesToDelete;
    }
}
