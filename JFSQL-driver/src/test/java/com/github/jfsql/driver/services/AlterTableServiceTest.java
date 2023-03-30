package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
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
    private FileNameCreator fileNameCreator;
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
    void testAlterTable_withNewTableName() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getNewTableName()).thenReturn("myTableEdited");
        when(tableFinder.getTableByName("myTable")).thenReturn(table);

        AlterTableService alterTableServiceSpy = Mockito.spy(alterTableService);
        doNothing().when(alterTableServiceSpy)
            .renameTable(any(AlterTableWrapper.class), any(Database.class), any(Table.class));

        int result = alterTableServiceSpy.alterTable(statement);

        verify(tableFinder, times(1)).getTableByName("myTable");
        verify(alterTableServiceSpy, times(1)).renameTable(any(AlterTableWrapper.class), any(Database.class),
            any(Table.class));
        assertEquals(0, result);
    }

    @Test
    void testAlterTable_withOldColumnName() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getOldColumnName()).thenReturn("col1");
        when(tableFinder.getTableByName("myTable")).thenReturn(table);

        AlterTableService alterTableServiceSpy = Mockito.spy(alterTableService);
        doNothing().when(alterTableServiceSpy)
            .renameColumn(any(AlterTableWrapper.class), any(Database.class), any(Table.class));

        int result = alterTableServiceSpy.alterTable(statement);

        verify(tableFinder, times(1)).getTableByName("myTable");
        verify(alterTableServiceSpy, times(1)).renameColumn(any(AlterTableWrapper.class), any(Database.class),
            any(Table.class));
        assertEquals(0, result);
    }

    @Test
    void testAlterTable_withColumnNameToAdd() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getColumnNameToAdd()).thenReturn("col1");
        when(tableFinder.getTableByName("myTable")).thenReturn(table);

        AlterTableService alterTableServiceSpy = Mockito.spy(alterTableService);
        doNothing().when(alterTableServiceSpy)
            .addColumn(any(AlterTableWrapper.class), any(Database.class), any(Table.class));

        int result = alterTableServiceSpy.alterTable(statement);

        verify(tableFinder, times(1)).getTableByName("myTable");
        verify(alterTableServiceSpy, times(1)).addColumn(any(AlterTableWrapper.class), any(Database.class),
            any(Table.class));
        assertEquals(0, result);
    }

    @Test
    void testAlterTable_withColumnToDrop() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getColumnToDrop()).thenReturn("col1");
        when(tableFinder.getTableByName("myTable")).thenReturn(table);

        AlterTableService alterTableServiceSpy = Mockito.spy(alterTableService);
        doNothing().when(alterTableServiceSpy)
            .dropColumn(any(AlterTableWrapper.class), any(Database.class), any(Table.class));

        int result = alterTableServiceSpy.alterTable(statement);

        verify(tableFinder, times(1)).getTableByName("myTable");
        verify(alterTableServiceSpy, times(1)).dropColumn(any(AlterTableWrapper.class), any(Database.class),
            any(Table.class));
        assertEquals(0, result);
    }

    @Test
    void testRenameTable_tableNameEqualsDatabaseName() throws SQLException {
        when(statement.getNewTableName()).thenReturn("myDatabase");
        when(semanticValidator.tableNameEqualsDatabaseName(statement.getNewTableName(), database)).thenReturn(true);
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameTable(statement, database, table));
        assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
        verify(transactionManager, never()).executeDDLOperation(any(), any(), any());
    }

    @Test
    void testRenameTable_whenFailedToRenameFilesThenRethrowException() throws IOException, SQLException {
        when(statement.getNewTableName()).thenReturn("myTableEdited");
        when(fileNameCreator.createTableFileName(statement.getNewTableName(), database)).thenReturn(
            "myTableEdited.xml");

        when(table.getTableFile()).thenReturn("tableFile");
        when(table.getSchema()).thenReturn(schema);
        when(schema.getSchemaFile()).thenReturn("schemaFile");
        when(transactionManager.getUncommittedTables()).thenReturn(uncommittedTables);
        when(uncommittedTables.contains(table)).thenReturn(false);

        doThrow(IOException.class).when(ioOperationHandler).renameFile(anyString(), anyString());
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameTable(statement, database, table));
        assertTrue(thrown.getMessage().contains("Failed to rename files.\n"));
        verify(transactionManager, never()).executeDDLOperation(any(), any(), any());
    }

    @Test
    void testRenameTable_whenTableIsNotCommittedButExistsThenDoNotThrowException() throws IOException, SQLException {
        when(statement.getNewTableName()).thenReturn("myTableEdited");
        when(fileNameCreator.createTableFileName(statement.getNewTableName(), database)).thenReturn(
            "myTableEdited.xml");

        when(table.getTableFile()).thenReturn("tableFile");
        when(table.getSchema()).thenReturn(schema);
        when(schema.getSchemaFile()).thenReturn("schemaFile");
        when(transactionManager.getUncommittedTables()).thenReturn(uncommittedTables);
        when(uncommittedTables.contains(table)).thenReturn(true);

        doThrow(IOException.class).when(ioOperationHandler).renameFile(anyString(), anyString());
        assertDoesNotThrow(() -> alterTableService.renameTable(statement, database, table));
        verify(transactionManager, times(1)).executeDDLOperation(any(), any(), any());
    }

    @Test
    void testRenameColumn_columnExists() throws SQLException {
        when(statement.getNewColumnName()).thenReturn("newColumnName");
        when(semanticValidator.columnIsPresentInTable(table, statement.getNewColumnName())).thenReturn(true);
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameColumn(statement, database, table));
        assertTrue(thrown.getMessage()
            .contains("The column '" + statement.getNewColumnName() + "' already exists in '" + table.getName() + "'"));
        verify(transactionManager, never()).executeDDLOperation(any(), any(), any());
    }
}
