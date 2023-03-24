package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
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

    public JGitTransactionManagerImpl(final Path url, final Reader reader, final Writer writer) throws SQLException {
        super(url, reader, writer);
    }

    @Override
    public void commit() throws SQLException {
        final File databaseDirectoryPath = database.getUrl().getParent().toFile();
        try (final Git git = Git.open(databaseDirectoryPath)) {
            writeUncommittedObjects();
            final Collection<File> filesToDelete = getFilesThatShouldNotBePresent();
            for (final File file : filesToDelete) {
                git.rm().addFilepattern(file.getName()).call();
            }
            git.add().addFilepattern(".").call();
            git.commit().setMessage("Commit").call();
        } catch (final GitAPIException | IOException e) {
            throw new SQLException("commit failed.\n " + e.getMessage());
        }
    }

    @Override
    public void rollback() throws SQLException {
        logger.warn("transaction rollback...");
        try (final Git git = Git.open(database.getUrl().getParent().toFile())) {
            final ResetCommand resetCommand = git.reset().setMode(ResetCommand.ResetType.HARD);
            resetCommand.call();
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final GitAPIException | IOException e) {
            throw new SQLException("There was an error executing the rollback.\n" + e.getMessage());
        }
    }

    private Collection<File> getFilesThatShouldNotBePresent() throws IOException {
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

    @Override
    public void openDatabase() throws SQLException {
        try (final Git ignored = Git.open(database.getUrl().getParent().toFile())) {
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final IOException e) {
            throw new SQLException("Couldn't open git repository.\n" + e.getMessage());
        }
    }

    @Override
    public void initDatabase(final Database database) throws SQLException {
        try (final Git git = Git.init().setDirectory(database.getUrl().getParent().toFile()).call()) {
            final List<Table> tables = new ArrayList<>();
            database.setTables(tables);
            writer.writeDatabaseFile(database);
            git.add().addFilepattern(".").call();
            git.commit().setMessage("Initial commit").call();
        } catch (final GitAPIException e) {
            throw new SQLException("Couldn't init git repository.\n" + e.getMessage());
        } catch (final IOException e) {
            throw new SQLException("Couldn't write database file.\n" + e.getMessage());
        }
    }
}
