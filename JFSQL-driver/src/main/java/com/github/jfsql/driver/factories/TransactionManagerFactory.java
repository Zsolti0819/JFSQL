package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.JGitTransactionManagerImpl;
import com.github.jfsql.driver.transactions.NotVersioningTransactionManagerImpl;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.PropertiesReader;
import java.nio.file.Path;
import java.sql.SQLException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class TransactionManagerFactory {

    public TransactionManager createTransactionManager(final PropertiesReader propertiesReader, final Path url,
        final Reader reader, final Writer writer) throws SQLException {
        final boolean useJgit = propertiesReader.isTransactionVersioning();
        if (useJgit) {
            return new JGitTransactionManagerImpl(url, reader, writer);
        } else {
            return new NotVersioningTransactionManagerImpl(url, reader, writer);
        }
    }
}
