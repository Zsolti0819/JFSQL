package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.util.FileNameCreator;
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
    private DatabaseManager databaseManager;
    @Mock
    private Reader reader;
    @Mock
    private FileNameCreator fileNameCreator;
    @Mock
    private CreateDatabaseWrapper createDatabaseStatement;
    @InjectMocks
    private CreateDatabaseService createDatabaseService;

    @Test
    void testCreateDatabase_normally() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(false);
        when(semanticValidator.databaseExist(createDatabaseStatement, reader.getFileExtension())).thenReturn(false);
        createDatabaseService.createDatabase(createDatabaseStatement);
        verify(databaseManager, times(1)).executeCreateDatabaseOperation(any());
    }

    @Test
    void testCreateDatabase_databaseIsNotDirectory() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> createDatabaseService.createDatabase(createDatabaseStatement));
        assertEquals("Database is not a directory.", thrown.getMessage());

        verify(databaseManager, never()).executeCreateDatabaseOperation(any());
    }

    @Test
    void testCreateDatabase_databaseExists() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(false);
        when(semanticValidator.databaseExist(createDatabaseStatement, reader.getFileExtension())).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> createDatabaseService.createDatabase(createDatabaseStatement));
        assertEquals("Database already exists.", thrown.getMessage());

        verify(databaseManager, never()).executeCreateDatabaseOperation(any());
    }
}
