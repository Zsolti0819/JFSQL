package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import lombok.RequiredArgsConstructor;

import java.io.File;
import java.nio.file.Path;
import java.sql.SQLException;

@RequiredArgsConstructor
public class CreateDatabaseService {

    private final Transaction transaction;
    private final SemanticValidator semanticValidator;
    private final Writer writer;

    public void createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        if (semanticValidator.urlIsAnExistingRegularFile(statement)) {
            throw new SQLException("Database is not a directory.");
        }

        if (semanticValidator.databaseExist(statement, writer.getFileExtension())) {
            throw new SQLException("Database already exists, will not create another one.");
        }

        final Path url = Path.of(statement.getDatabaseUrl());
        final String fileName = File.separator + url.getFileName() + "." + writer.getFileExtension();
        final Path databaseFilePath = Path.of(url + fileName);
        final Database database = new Database(databaseFilePath);

        transaction.executeCreateDatabaseOperation(database);
    }
}
