package com.github.jfsql.driver.services;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.SharedMapHandler;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class CreateTableService {

    private static final Logger logger = LogManager.getLogger(CreateTableService.class);
    private final DatabaseManager databaseManager;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final FileNameCreator fileNameCreator;

    int createTable(final CreateTableWrapper statement) throws SQLException {
        final Database database = databaseManager.getDatabase();
        final String tableName = statement.getTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(statement.getTableName(), database)) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final boolean ifNotExistsIsPresent = statement.isIfNotExistsPresent();

        if (!ifNotExistsIsPresent && semanticValidator.tableExists(statement, database)) {
            throw new SQLException("Table '" + tableName + "' already exists.");
        }

        if (ifNotExistsIsPresent && semanticValidator.tableExists(statement, database)) {
            logger.debug(
                "Table '{}' already exists, but 'IF NOT EXISTS' clause was present in the statement, no new table will be created.",
                tableName);
            return 0;
        }

        if (semanticValidator.statementColumnsContainDuplicates(statement)) {
            throw new SQLException("Duplicate columns were found in the statement.");
        }

        final List<String> columns = statement.getColumns();
        final List<String> types = statement.getTypes();

        final LinkedHashMap<String, String> columnsAndTypes = columns.stream()
            .collect(Collectors.toMap(Function.identity(), k -> types.get(columns.indexOf(k)), (v1, v2) -> v1,
                LinkedHashMap::new));

        final Map<String, Boolean> notNulLColumns = statement.getNotNullColumns();
        final String tableFile = fileNameCreator.createTableFileName(tableName, database);
        final String schemaFile = fileNameCreator.createSchemaFileName(tableName, database);

        final Table table = Table.builder()
            .name(tableName)
            .tableFile(tableFile)
            .schemaFile(schemaFile)
            .columnsAndTypes(columnsAndTypes)
            .notNullColumns(notNulLColumns)
            .entries(new ArrayList<>())
            .build();

        SharedMapHandler.addSchematoSharedMap(table);
        SharedMapHandler.addTableToSharedMap(table);

        logger.debug("table created = {}", table);

        final List<Table> tables = database.getTables();
        tables.add(table);
        transactionManager.executeOperation(database, table);
        return 0;
    }
}
