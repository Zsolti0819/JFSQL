package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import lombok.RequiredArgsConstructor;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.sql.SQLException;

@RequiredArgsConstructor
class DropDatabaseService {

    private final StatementManager statementManager;
    private final Writer writer;
    private final SemanticValidator semanticValidator;

    int dropDatabase(final DropDatabaseWrapper statement) throws SQLException {
        if (!semanticValidator.databaseExist(statement, writer.getFileExtension())) {
            throw new SQLException("Database file does not exist, it cannot be deleted.");
        }

        if (!FileUtils.deleteQuietly(new File(statement.getDatabaseUrl()))) {
            throw new SQLException("Couldn't drop database.");
        }

        statementManager.setDatabase(null);
        return 1;
    }
}
