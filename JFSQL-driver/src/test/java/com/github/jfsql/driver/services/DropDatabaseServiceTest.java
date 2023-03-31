package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import java.sql.SQLException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DropDatabaseServiceTest {

    @Mock
    private DatabaseManager databaseManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private IoOperationHandler ioOperationHandler;

    @Mock
    private Reader reader;

    @Mock
    private DropDatabaseWrapper statement;

    @InjectMocks
    private DropDatabaseService dropDatabaseService;

    @Test
    void testDropDatabase() throws SQLException {
        when(semanticValidator.databaseExist(statement, reader.getFileExtension())).thenReturn(true);
        when(ioOperationHandler.databaseDroppedSuccessfully(statement)).thenReturn(true);

        final int result = dropDatabaseService.dropDatabase(statement);

        assertEquals(1, result);
        verify(databaseManager, times(1)).setDatabase(null);
    }

    @Test
    void testDropDatabase_databaseDoesNotExist() {
        when(semanticValidator.databaseExist(statement, reader.getFileExtension())).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class, () -> dropDatabaseService.dropDatabase(
            statement));
        assertEquals("Database file does not exist, it cannot be deleted.", thrown.getMessage());
        verify(ioOperationHandler, never()).databaseDroppedSuccessfully(statement);
        verify(databaseManager, never()).setDatabase(null);
    }

    @Test
    void testDropDatabase_failedToDropDatabase() {

        when(semanticValidator.databaseExist(statement, reader.getFileExtension())).thenReturn(true);
        when(ioOperationHandler.databaseDroppedSuccessfully(statement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class, () -> dropDatabaseService.dropDatabase(
            statement));
        assertEquals("Failed to DROP the database.", thrown.getMessage());
        verify(databaseManager, never()).setDatabase(null);
    }
}
