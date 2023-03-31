package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.JGitTransactionManagerImpl;
import com.github.jfsql.driver.db.NotVersioningTransactionManagerImpl;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import lombok.experimental.UtilityClass;

@UtilityClass
public class TransactionManagerFactory {

    public TransactionManager createTransactionManager(final PropertiesReader propertiesReader,
        final DatabaseManager databaseManager,
        final Reader reader, final Writer writer) {
        final boolean useJgit = propertiesReader.isTransactionVersioning();
        if (useJgit) {
            return new JGitTransactionManagerImpl(databaseManager, reader, writer);
        } else {
            return new NotVersioningTransactionManagerImpl(databaseManager, reader, writer);
        }
    }
}
