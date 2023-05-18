package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.config.TransactionVersioning;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.DefaultDatabaseManagerImpl;
import com.github.jfsql.driver.db.JGitDatabaseManagerImpl;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import java.sql.SQLException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DatabaseManagerFactory {

    public DatabaseManager createDatabaseManager(final PropertiesReader propertiesReader, final String URL,
        final Reader reader, final Writer writer) throws SQLException {
        final TransactionVersioning type = propertiesReader.getTransactionVersioning();
        switch (type) {
            case JGIT:
                return new JGitDatabaseManagerImpl(URL, reader, writer);
            case DEFAULT:
                return new DefaultDatabaseManagerImpl(URL, reader, writer);
            default:
                throw new IllegalArgumentException("Unknown TransactionVersioning type '" + type);
        }
    }

}
