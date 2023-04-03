package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.JGitDatabaseManagerImpl;
import com.github.jfsql.driver.db.NotVersioningDatabaseManagerImpl;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.sql.SQLException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DatabaseManagerFactory {

    public DatabaseManager createDatabaseManager(final PropertiesReader propertiesReader, final String url,
        final Reader reader, final Writer writer) throws SQLException {
        final boolean useJgit = propertiesReader.isTransactionVersioning();
        if (useJgit) {
            return new JGitDatabaseManagerImpl(url, reader, writer);
        } else {
            return new NotVersioningDatabaseManagerImpl(url, reader, writer);
        }
    }

}
