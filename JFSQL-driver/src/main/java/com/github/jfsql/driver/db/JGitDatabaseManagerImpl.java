package com.github.jfsql.driver.db;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.validation.SemanticValidator;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;

public class JGitDatabaseManagerImpl extends DatabaseManager {

    public JGitDatabaseManagerImpl(final String URL, final SemanticValidator semanticValidator,
        final FileNameCreator fileNameCreator, final Reader reader, final Writer writer) throws SQLException {
        super(URL, semanticValidator, fileNameCreator, reader, writer);
    }

    @Override
    public void initDatabase(final Database database) throws SQLException {
        final Path databaseFolder = database.getURL().getParent();
        final Path blobFolder = Path.of(databaseFolder + File.separator + "blob");
        try (final Git git = Git.init().setDirectory(databaseFolder.toFile()).call()) {
            Files.createDirectories(blobFolder);
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

    @Override
    public void openDatabase() throws SQLException {
        final Path databaseFolder = database.getURL().getParent();
        try (final Git ignored = Git.open(databaseFolder.toFile())) {
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final IOException e) {
            throw new SQLException("Couldn't open git repository.\n" + e.getMessage());
        }
    }

}
