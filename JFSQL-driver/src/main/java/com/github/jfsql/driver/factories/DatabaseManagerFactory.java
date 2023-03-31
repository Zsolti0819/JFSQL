package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.JGitDatabaseManagerImpl;
import com.github.jfsql.driver.transactions.NotVersioningDatabaseManagerImpl;
import java.nio.file.Path;
import java.sql.SQLException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DatabaseManagerFactory {

    public DatabaseManager createTransactionManager(final PropertiesReader propertiesReader, final Path url,
        final Reader reader, final Writer writer) throws SQLException {
        final boolean useJgit = propertiesReader.isTransactionVersioning();
        if (useJgit) {
            return new JGitDatabaseManagerImpl(url, reader, writer);
        } else {
            return new NotVersioningDatabaseManagerImpl(url, reader, writer);
        }
    }

}
