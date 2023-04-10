package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.DefaultTransactionManagerImpl;
import com.github.jfsql.driver.db.JGitTransactionManagerImpl;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class TransactionManagerFactory {

    public TransactionManager createTransactionManager(final PropertiesReader propertiesReader,
        final DatabaseManager databaseManager,
        final Reader reader, final Writer writer) {
        final String type = propertiesReader.getTransactionVersioning();
        if (Objects.equals(type, "jgit")) {
            return new JGitTransactionManagerImpl(databaseManager, reader, writer);
        } else {
            return new DefaultTransactionManagerImpl(databaseManager, reader, writer);
        }
    }
}
