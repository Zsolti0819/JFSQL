package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.CommittingTransaction;
import com.github.jfsql.driver.transactions.NotCommittingTransaction;
import com.github.jfsql.driver.transactions.Transaction;
import lombok.experimental.UtilityClass;

import java.util.Objects;

@UtilityClass
public class TransactionFactory {

    public Transaction createTransactionManager(final String type, final Database database, final Reader reader,
                                                final Writer writer) {
        if (Objects.equals("committing", type)) {
            return CommittingTransaction.builder()
                    .database(database)
                    .reader(reader)
                    .writer(writer)
                    .autoCommit(true)
                    .build();
        } else if (Objects.equals("not committing", type)) {
            return NotCommittingTransaction.builder()
                    .database(database)
                    .reader(reader)
                    .writer(writer)
                    .autoCommit(true)
                    .build();
        } else {
            throw new IllegalArgumentException("Unknown Transaction type");
        }
    }
}
