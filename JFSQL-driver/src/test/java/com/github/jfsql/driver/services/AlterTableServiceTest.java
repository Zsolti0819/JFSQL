package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AlterTableServiceTest {

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Reader reader;

    @Mock
    private Database database;

    @Mock
    private Table table;

    @Mock
    private AlterTableStatement statement;

    @InjectMocks
    private AlterTableService alterTableService;

    @Test
    void testAlterTable_withNewTableName() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getNewTableName()).thenReturn("myTableEdited");
        when(database.getURL()).thenReturn("someUrl");
        when(reader.getFileExtension()).thenReturn("someExtension");

        final AlterTableService alterTableServiceSpy = spy(alterTableService);
        final int result = alterTableServiceSpy.alterTable(statement);

        verify(alterTableServiceSpy, times(1)).renameTable(any(), any(), any());
        assertEquals(0, result);
    }

    @Test
    void testAlterTable_withOldColumnName() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getOldColumnName()).thenReturn("col1");

        final AlterTableService alterTableServiceSpy = spy(alterTableService);
        final int result = alterTableServiceSpy.alterTable(statement);

        verify(alterTableServiceSpy, times(1)).renameColumn(any(), any());
        assertEquals(0, result);
    }

    @Test
    void testAlterTable_withColumnNameToAdd() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getColumnNameToAdd()).thenReturn("col1");

        final AlterTableService alterTableServiceSpy = spy(alterTableService);
        final int result = alterTableServiceSpy.alterTable(statement);

        verify(alterTableServiceSpy, times(1)).addColumn(any(), any());
        assertEquals(0, result);
    }

    @Test
    void testAlterTable_withColumnToDrop() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getColumnToDrop()).thenReturn("id");
        when(semanticValidator.columnIsPresentInTable(table, statement.getColumnToDrop())).thenReturn(true);

        final AlterTableService alterTableServiceSpy = spy(alterTableService);
        final int result = alterTableServiceSpy.alterTable(statement);

        verify(alterTableServiceSpy, times(1)).dropColumn(any(), any());
        assertEquals(0, result);
    }

    @Test
    void testRenameTable_tableNameEqualsDatabaseName() throws SQLException {
        when(statement.getNewTableName()).thenReturn("myDatabase");
        when(semanticValidator.tableNameEqualsDatabaseName(statement.getNewTableName(), database)).thenReturn(true);
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameTable(statement, database, table));
        assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
        verify(transactionManager, never()).execute(any(), any(), any());
    }

    @Test
    void testRenameColumn_columnExists() throws SQLException {
        when(statement.getNewColumnName()).thenReturn("newColumnName");
        when(semanticValidator.columnIsPresentInTable(table, statement.getNewColumnName())).thenReturn(true);
        final SQLException thrown = assertThrows(SQLException.class,
            () -> alterTableService.renameColumn(statement, table));
        assertTrue(thrown.getMessage()
            .contains("The column '" + statement.getNewColumnName() + "' already exists in '" + table.getName() + "'"));
        verify(transactionManager, never()).execute(any(), any(), any());
    }
}
