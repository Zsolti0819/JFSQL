package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.config.TransactionVersioning;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.DefaultDatabaseManager;
import com.github.jfsql.driver.db.JGitDatabaseManagerImpl;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.validation.SemanticValidator;
import java.sql.SQLException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DatabaseManagerFactory {

    public DatabaseManager createDatabaseManager(final PropertiesReader propertiesReader, final String URL,
        final SemanticValidator semanticValidator, final FileNameCreator fileNameCreator, final Reader reader,
        final Writer writer) throws SQLException {
        final TransactionVersioning type = propertiesReader.getTransactionVersioning();
        switch (type) {
            case JGIT:
                return new JGitDatabaseManagerImpl(URL, semanticValidator, fileNameCreator, reader, writer);
            case DEFAULT:
                return new DefaultDatabaseManager(URL, semanticValidator, fileNameCreator, reader, writer);
            default:
                throw new IllegalArgumentException("Unknown TransactionVersioning type '" + type);
        }
    }

}
