package com.github.jfsql.driver.services;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CreateTableServiceTest {

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Writer writer;

    @Mock
    private Database database;

    @Mock
    private CreateTableWrapper createTableStatement;

    @InjectMocks
    private CreateTableService createTableService;

    @Test
    void testCreateTable_normally() throws SQLException {
        when(writer.getFileExtension()).thenReturn("xml");
        when(database.getUrl()).thenReturn(TestUtils.DATABASE_XML_FILE_PATH);
        when(semanticValidator.tableExists(createTableStatement, database)).thenReturn(false);

        createTableService.createTable(createTableStatement);

        verify(semanticValidator, times(1)).columnsHaveDuplicate(any());
        verify(database, times(2)).getTables();
        verify(transactionManager, times(1)).executeDDLOperation(any());

    }

    @Test
    void testCreateTable_tableNameAndDatabaseNameAreEqual() throws SQLException {
        when(semanticValidator.tableNameEqualsDatabaseName(createTableStatement.getTableName(), database)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class, () -> createTableService.createTable(createTableStatement));
        Assertions.assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
        verify(transactionManager, never()).executeDDLOperation(any());
    }

    @Test
    void testCreateTable_tableExists() throws SQLException {
        when(createTableStatement.isIfNotExistsPresent()).thenReturn(false);
        when(semanticValidator.tableExists(createTableStatement, database)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class, () -> createTableService.createTable(createTableStatement));
        Assertions.assertEquals("Table \"" + createTableStatement.getTableName() + "\" already exists.", thrown.getMessage());
        verify(transactionManager, never()).executeDDLOperation(any());
    }

    @Test
    void testCreateTable_ifNotExists_doesNotThrowException() throws SQLException {
        when(createTableStatement.isIfNotExistsPresent()).thenReturn(true);
        when(semanticValidator.tableExists(createTableStatement, database)).thenReturn(true);

        assertDoesNotThrow(() -> createTableService.createTable(createTableStatement));
        verify(transactionManager, never()).executeDDLOperation(any());
    }

    @Test
    void testCreateTable_identicalColumns() throws SQLException {
        when(semanticValidator.tableExists(createTableStatement, database)).thenReturn(false);
        when(semanticValidator.columnsHaveDuplicate(createTableStatement)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class, () -> createTableService.createTable(createTableStatement));
        assertEquals("Some columns were identical during table creation.", thrown.getMessage());

        verify(transactionManager, never()).executeDDLOperation(any());
    }

}
