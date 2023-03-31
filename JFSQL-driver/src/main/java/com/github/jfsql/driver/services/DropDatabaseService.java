package com.github.jfsql.driver.services;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import java.io.File;
import java.sql.SQLException;
import lombok.RequiredArgsConstructor;
import org.apache.commons.io.FileUtils;

@RequiredArgsConstructor
class DropDatabaseService {

    private final DatabaseManager databaseManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    int dropDatabase(final DropDatabaseWrapper statement) throws SQLException {
        if (!semanticValidator.databaseExist(statement, reader.getFileExtension())) {
            throw new SQLException("Database file does not exist, it cannot be deleted.");
        }

        if (!FileUtils.deleteQuietly(new File(statement.getDatabaseUrl()))) {
            throw new SQLException("Couldn't drop database.");
        }

        databaseManager.setDatabase(null);
        return 1;
    }
}
