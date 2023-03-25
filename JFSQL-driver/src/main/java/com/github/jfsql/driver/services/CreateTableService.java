package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import java.io.File;
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
    private final Reader reader;

    public int createTable(final CreateTableWrapper statement) throws SQLException {
        final Database database = databaseManager.getDatabase();
        final String tableName = statement.getTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(statement.getTableName(), database)) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final boolean ifNotExistsIsPresent = statement.isIfNotExistsPresent();

        if (!ifNotExistsIsPresent && semanticValidator.tableExists(statement, database)) {
            throw new SQLException("Table \"" + tableName + "\" already exists.");
        }

        if (ifNotExistsIsPresent && semanticValidator.tableExists(statement, database)) {
            logger.debug(
                "Table '{}' already exists, but 'IF NOT EXISTS' clause was present in the statement, no new table will be created.",
                tableName);
            return 0;
        }

        if (semanticValidator.columnsHaveDuplicate(statement)) {
            throw new SQLException("Some columns were identical during table creation.");
        }

        final List<String> columns = statement.getColumns();
        final List<String> types = statement.getTypes();

        final LinkedHashMap<String, String> columnsAndTypes = columns.stream()
            .collect(Collectors.toMap(Function.identity(), k -> types.get(columns.indexOf(k)), (v1, v2) -> v1,
                LinkedHashMap::new));

        final Map<String, Boolean> notNulLColumns = statement.getNotNullColumns();
        final String parentDirectory = String.valueOf(database.getUrl().getParent());
        final String tableFile = parentDirectory + File.separator + tableName + "." + reader.getFileExtension();
        final String schemaFile =
            reader instanceof ReaderJsonImpl ? parentDirectory + File.separator + tableName + "Schema."
                + reader.getSchemaFileExtension()
                : parentDirectory + File.separator + tableName + "." + reader.getSchemaFileExtension();

        final Schema schema = new Schema(schemaFile, columnsAndTypes, notNulLColumns);
        final Table table = new Table(tableName, tableFile, schema, new ArrayList<>());
        final List<Entry> entries = new ArrayList<>();
        table.setEntries(entries);
        database.getTables().add(table);
        transactionManager.executeDDLOperation(database, table, table.getSchema());
        return 0;
    }
}
