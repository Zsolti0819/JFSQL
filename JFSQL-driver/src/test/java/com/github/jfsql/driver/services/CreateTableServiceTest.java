package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import java.nio.file.Path;
import java.sql.SQLException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CreateTableServiceTest {

    @Mock
    private DatabaseManager databaseManager;

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Database database;

    @Mock
    private Reader reader;

    @Mock
    private CreateTableWrapper statement;


    @InjectMocks
    private CreateTableService createTableService;

    @Test
    void testCreateTable_normally() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(semanticValidator.tableExists(statement, database)).thenReturn(false);
        when(statement.getTableName()).thenReturn("myTable");
        when(database.getURL()).thenReturn(Path.of("someUrl"));
        when(reader.getFileExtension()).thenReturn("someExtension");

        createTableService.createTable(statement);

        verify(semanticValidator, times(1)).statementColumnsContainDuplicates(any());
        verify(database, times(1)).getTables();
        verify(transactionManager, times(1)).execute(any(), any(), any());

    }

    @Test
    void testCreateTable_tableNameAndDatabaseNameAreEqual() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(semanticValidator.tableNameEqualsDatabaseName(statement.getTableName(), database)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> createTableService.createTable(statement));
        Assertions.assertEquals("Table name cannot be the same as database name.", thrown.getMessage());
        verify(transactionManager, never()).execute(any(), any(), any());
    }

    @Test
    void testCreateTable_tableExists() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(statement.isIfNotExistsPresent()).thenReturn(false);
        when(semanticValidator.tableExists(statement, database)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class, () -> createTableService.createTable(
            statement));
        Assertions.assertEquals("Table '" + statement.getTableName() + "' already exists.", thrown.getMessage());
        verify(transactionManager, never()).execute(any(), any(), any());
    }

    @Test
    void testCreateTable_ifNotExists_doesNotThrowException() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(statement.isIfNotExistsPresent()).thenReturn(true);
        when(semanticValidator.tableExists(statement, database)).thenReturn(true);

        assertDoesNotThrow(() -> createTableService.createTable(statement));
        verify(transactionManager, never()).execute(any(), any(), any());
    }

    @Test
    void testCreateTable_identicalColumns() throws SQLException {
        when(databaseManager.getDatabase()).thenReturn(database);
        when(semanticValidator.tableExists(statement, database)).thenReturn(false);
        when(semanticValidator.statementColumnsContainDuplicates(statement)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class, () -> createTableService.createTable(
            statement));
        assertEquals("Duplicate columns were found in the statement.", thrown.getMessage());

        verify(transactionManager, never()).execute(any(), any(), any());
    }

}
