package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.LinkedList;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class CreateDatabaseService {

    private final DatabaseManager databaseManager;
    private final SemanticValidator semanticValidator;
    private final FileNameCreator fileNameCreator;
    private final Reader reader;

    int createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        if (semanticValidator.urlIsAnExistingRegularFile(statement)) {
            throw new SQLException("Database is not a directory.");
        }

        if (semanticValidator.databaseExist(statement, reader.getFileExtension())) {
            throw new SQLException("Database already exists.");
        }

        final Path databaseFilePath = fileNameCreator.createDatabaseFileName(statement);
        final Database database = new Database(databaseFilePath, new LinkedList<>());

        databaseManager.executeCreateDatabaseOperation(database);
        return 0;
    }

}
