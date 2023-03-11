package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CreateDatabaseServiceTest {

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Writer writer;

    @Mock
    private StatementManager statementManager;

    @Mock
    private CreateDatabaseWrapper createDatabaseStatement;

    @InjectMocks
    private CreateDatabaseService createDatabaseService;

    @Test
    void testCreateDatabase_normally() throws SQLException {
        when(createDatabaseStatement.getDatabaseUrl()).thenReturn(String.valueOf(TestUtils.DATABASE_PATH));
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(false);
        when(semanticValidator.databaseExist(createDatabaseStatement, writer.getFileExtension())).thenReturn(false);
        createDatabaseService.createDatabase(createDatabaseStatement);
        verify(statementManager, times(1)).executeCreateDatabaseOperation(any());
    }

    @Test
    void testCreateDatabase_databaseIsNotDirectory() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
                () -> createDatabaseService.createDatabase(createDatabaseStatement));
        assertEquals("Database is not a directory.", thrown.getMessage());

        verify(statementManager, never()).executeCreateDatabaseOperation(any());
    }

    @Test
    void testCreateDatabase_databaseExists() throws SQLException {
        when(semanticValidator.urlIsAnExistingRegularFile(createDatabaseStatement)).thenReturn(false);
        when(semanticValidator.databaseExist(createDatabaseStatement, writer.getFileExtension())).thenReturn(true);

        final SQLException thrown = assertThrows(SQLException.class,
                () -> createDatabaseService.createDatabase(createDatabaseStatement));
        assertEquals("Database already exists, will not create another one.", thrown.getMessage());

        verify(statementManager, never()).executeCreateDatabaseOperation(any());
    }
}