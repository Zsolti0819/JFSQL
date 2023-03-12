package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.CommittingTransaction;
import com.github.jfsql.driver.transactions.NotCommittingTransaction;
import com.github.jfsql.driver.transactions.Transaction;
import lombok.experimental.UtilityClass;

import java.nio.file.Path;
import java.sql.SQLException;
import java.util.Objects;

@UtilityClass
public class TransactionFactory {

    public Transaction createTransactionManager(final String type, final Path url, final Reader reader,
                                                final Writer writer) throws SQLException {
        if (Objects.equals("committing", type)) {
            return new CommittingTransaction(url, reader, writer);
        } else if (Objects.equals("not committing", type)) {
            return new NotCommittingTransaction(url, reader, writer);
        } else {
            throw new IllegalArgumentException("Unknown Transaction type");
        }
    }
}
