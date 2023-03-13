package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import lombok.RequiredArgsConstructor;

import java.io.File;
import java.nio.file.Path;
import java.sql.SQLException;

@RequiredArgsConstructor
public class CreateDatabaseService {

    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    public void createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        if (semanticValidator.urlIsAnExistingRegularFile(statement)) {
            throw new SQLException("Database is not a directory.");
        }

        if (semanticValidator.databaseExist(statement, reader.getFileExtension())) {
            throw new SQLException("Database already exists, will not create another one.");
        }

        final Path url = Path.of(statement.getDatabaseUrl());
        final String fileName = File.separator + url.getFileName() + "." + reader.getFileExtension();
        final Path databaseFilePath = Path.of(url + fileName);
        final Database database = new Database(databaseFilePath);

        transactionManager.executeCreateDatabaseOperation(database);
    }
}
