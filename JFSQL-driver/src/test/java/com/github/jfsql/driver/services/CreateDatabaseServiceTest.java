package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import java.sql.SQLException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CreateDatabaseServiceTest {

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private Reader reader;

    @Mock
    private CreateDatabaseWrapper createDatabaseStatement;

    @InjectMocks
    private CreateDatabaseService createDatabaseService;

    @Test
    void testCreateDatabase_normally() throws SQLException {
        when(createDatabaseStatement.getDatabaseUrl()).thenReturn(String.valueOf(TestUtils.DATABASE_PATH));
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(false);
        when(semanticValidator.databaseExist(createDatabaseStatement, reader.getFileExtension())).thenReturn(false);
        createDatabaseService.createDatabase(createDatabaseStatement);
        verify(transactionManager, times(1)).executeCreateDatabaseOperation(any());
    }

    @Test
    void testCreateDatabase_databaseIsNotDirectory() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> createDatabaseService.createDatabase(createDatabaseStatement));
        assertEquals("Database is not a directory.", thrown.getMessage());

        verify(transactionManager, never()).executeCreateDatabaseOperation(any());
    }

    @Test
    void testCreateDatabase_databaseExists() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(false);
        when(semanticValidator.databaseExist(createDatabaseStatement, reader.getFileExtension())).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> createDatabaseService.createDatabase(createDatabaseStatement));
        assertEquals("Database already exists, will not create another one.", thrown.getMessage());

        verify(transactionManager, never()).executeCreateDatabaseOperation(any());
    }
}