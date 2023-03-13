package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.JGitTransactionManagerImpl;
import com.github.jfsql.driver.transactions.NotVersioningTransactionManagerImpl;
import com.github.jfsql.driver.transactions.TransactionManager;
import lombok.experimental.UtilityClass;

import java.nio.file.Path;
import java.sql.SQLException;
import java.util.Objects;

@UtilityClass
public class TransactionManagerFactory {

    public TransactionManager createTransactionManager(final String type, final Path url, final Reader reader,
                                                       final Writer writer) throws SQLException {
        if (Objects.equals("true", type)) {
            return new JGitTransactionManagerImpl(url, reader, writer);
        } else if (Objects.equals("false", type)) {
            return new NotVersioningTransactionManagerImpl(url, reader, writer);
        } else {
            throw new IllegalArgumentException("Unknown TransactionManager type");
        }
    }
}
