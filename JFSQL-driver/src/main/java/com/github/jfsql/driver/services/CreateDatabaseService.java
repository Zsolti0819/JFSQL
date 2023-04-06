package com.github.jfsql.driver.services;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.LinkedList;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class CreateDatabaseService {

    private static final Logger logger = LogManager.getLogger(CreateDatabaseService.class);
    private final DatabaseManager databaseManager;
    private final SemanticValidator semanticValidator;
    private final FileNameCreator fileNameCreator;
    private final Reader reader;

    int createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        final String URL = statement.getDatabaseURL();

        if (semanticValidator.URLIsNotDirectory(URL)) {
            throw new SQLException("Database is not a directory.");
        }

        if (semanticValidator.databaseExist(statement, reader.getFileExtension())) {
            throw new SQLException("Database already exists.");
        }

        final Path databaseFilePath = fileNameCreator.createDatabaseFileName(URL);
        final Database database = new Database(databaseFilePath, new LinkedList<>());

        logger.debug("database object created = {}", database);

        databaseManager.executeCreateDatabaseOperation(database);
        return 0;
    }

}
