package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AlterTableServiceTest {

    @Mock
    private TableFinder tableFinder;
    @Mock
    private DatabaseManager databaseManager;
    @Mock
    private TransactionManager transactionManager;
    @Mock
    private SemanticValidator semanticValidator;
    @Mock
    private IoOperationHandler ioOperationHandler;
    @Mock
    private Reader reader;
    @Mock
    private Database database;
    @Mock
    private Table table;
    @Mock
    private Schema schema;
    @Mock
    private AlterTableWrapper statement;
    @Mock
    private Set<Table> uncommittedTables;
    @InjectMocks
    private AlterTableService alterTableService;

    @Test
    void testRenameTable_tableNameEqualsDatabaseName() {
        when(statement.getNewTableName()).thenReturn("myDatabase");
        when(semanticValidator.tableNameEqualsDatabaseName(statement.getNewTableName(), database)).thenReturn(true);
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameTable(statement, database, table));
        assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
    }

    @Test
    void testRenameTable_whenFailedToRenameFilesThenRethrowException() throws IOException {
        when(statement.getNewTableName()).thenReturn("myTableEdited");
        when(reader.getFileExtension()).thenReturn("xml");
        when(reader.getSchemaFileExtension()).thenReturn("xsd");
        when(database.getUrl()).thenReturn(Path.of("someUrl"));
        when(table.getTableFile()).thenReturn("tableFile");
        when(table.getSchema()).thenReturn(schema);
        when(schema.getSchemaFile()).thenReturn("schemaFile");
        when(transactionManager.getUncommittedTables()).thenReturn(uncommittedTables);
        when(uncommittedTables.contains(table)).thenReturn(false);
        doThrow(IOException.class).when(ioOperationHandler).renameFile(anyString(), anyString());
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameTable(statement, database, table));
        assertTrue(thrown.getMessage().contains("Failed to rename files.\n"));
    }
}
