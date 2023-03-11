package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import lombok.RequiredArgsConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@RequiredArgsConstructor
public class CreateTableService {

    private final StatementManager statementManager;
    private final SemanticValidator semanticValidator;
    private final Writer writer;
    private static final Logger logger = LogManager.getLogger(CreateTableService.class);

    public void createTable(final CreateTableWrapper statement) throws SQLException {
        final String tableName = statement.getTableName();

        if (semanticValidator.tableNameEqualsDatabaseName(statement.getTableName(), statementManager.getDatabase())) {
            throw new SQLException("Table name cannot be the same as database name.");
        }

        final boolean ifNotExistsIsPresent = statement.isIfNotExistsPresent();

        if (!ifNotExistsIsPresent && semanticValidator.tableExists(statement, statementManager.getDatabase())) {
            throw new SQLException("Table \"" + tableName + "\" already exists.");
        }

        if (ifNotExistsIsPresent && semanticValidator.tableExists(statement, statementManager.getDatabase())) {
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
        final String parentDirectory = String.valueOf(statementManager.getDatabase().getUrl().getParent());
        final String tableFile = parentDirectory + File.separator + tableName + "." + writer.getFileExtension();
        final String schemaFile =
                writer instanceof WriterJsonImpl ? parentDirectory + File.separator + tableName + "Schema."
                        + writer.getSchemaFileExtension()
                        : parentDirectory + File.separator + tableName + "." + writer.getSchemaFileExtension();

        if (statementManager.getDatabase().getTables() == null) {
            statementManager.getDatabase().setTables(new ArrayList<>());
        }

        final Table table = new Table(tableName, tableFile, schemaFile, columnsAndTypes, notNulLColumns);
        final List<Entry> entries = new ArrayList<>();
        table.setEntries(entries);
        statementManager.getDatabase().getTables().add(table);
        statementManager.executeDDLOperation(table);
    }
}
