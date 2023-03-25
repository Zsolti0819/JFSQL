package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.JGitTransactionManagerImpl;
import com.github.jfsql.driver.transactions.NotVersioningTransactionManagerImpl;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.PropertiesReader;
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
