package com.github.jfsql.driver.transactions;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;

public class JGitDatabaseManagerImpl extends DatabaseManager {

    public JGitDatabaseManagerImpl(final Path url, final Reader reader, final Writer writer)
        throws SQLException {
        super(url, reader, writer);
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

    @Override
    public void openDatabase() throws SQLException {
        try (final Git ignored = Git.open(database.getUrl().getParent().toFile())) {
            final List<Table> tables = reader.readTablesFromDatabaseFile(database);
            database.setTables(tables);
        } catch (final IOException e) {
            throw new SQLException("Couldn't open git repository.\n" + e.getMessage());
        }
    }

}
