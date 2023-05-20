package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DefaultTransactionManagerImpl;
import com.github.jfsql.driver.db.JGitTransactionManagerImpl;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.enums.TransactionVersioning;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import lombok.experimental.UtilityClass;

@UtilityClass
public class TransactionManagerFactory {

    public TransactionManager createTransactionManager(final PropertiesReader propertiesReader,
        final Database database, final Reader reader, final Writer writer) {
        final TransactionVersioning type = propertiesReader.getTransactionVersioning();
        switch (type) {
            case JGIT:
                return new JGitTransactionManagerImpl(database, reader, writer);
            case DEFAULT:
                return new DefaultTransactionManagerImpl(database, reader, writer);
            default:
                throw new IllegalArgumentException("Unknown TransactionVersioning type '" + type);
        }
    }
}
