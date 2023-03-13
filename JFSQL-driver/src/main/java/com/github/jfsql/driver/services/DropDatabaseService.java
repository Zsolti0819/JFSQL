package com.github.jfsql.driver.services;

import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import lombok.RequiredArgsConstructor;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.sql.SQLException;

@RequiredArgsConstructor
class DropDatabaseService {

    private final Transaction transaction;
    private final SemanticValidator semanticValidator;
    private final Writer writer;

    int dropDatabase(final DropDatabaseWrapper statement) throws SQLException {
        if (!semanticValidator.databaseExist(statement, writer.getFileExtension())) {
            throw new SQLException("Database file does not exist, it cannot be deleted.");
        }

        if (!FileUtils.deleteQuietly(new File(statement.getDatabaseUrl()))) {
            throw new SQLException("Couldn't drop database.");
        }

        transaction.setDatabase(null);
        return 1;
    }
}
