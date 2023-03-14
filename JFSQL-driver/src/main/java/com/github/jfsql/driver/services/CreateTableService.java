package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
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
    private final Database database;
    private final TransactionManager transactionManager;
    private final SemanticValidator semanticValidator;
    private final Reader reader;

    public void createTable(final CreateTableWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(statement.getTableName(), database)) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final boolean ifNotExistsIsPresent = statement.isIfNotExistsPresent();

        if (!ifNotExistsIsPresent && semanticValidator.tableExists(statement, database)) {
            throw new SQLException("Table \"" + tableName + "\" already exists.");
        }

        if (ifNotExistsIsPresent && semanticValidator.tableExists(statement, database)) {
            logger.info(
                "Table '{}' already exists, but 'IF NOT EXISTS' clause was present in the statement, no new table will be created.",
                tableName);
            return;
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

        if (database.getTables() == null) {
            database.setTables(new ArrayList<>());
        }

        final Table table = new Table(tableName, tableFile, schemaFile, columnsAndTypes, notNulLColumns);
        final List<Entry> entries = new ArrayList<>();
        table.setEntries(entries);
        database.getTables().add(table);
        transactionManager.executeDDLOperation(table);
    }
}
