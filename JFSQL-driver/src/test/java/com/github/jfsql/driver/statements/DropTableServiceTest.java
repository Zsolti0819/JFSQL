package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropTableWrapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.SQLException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DropTableServiceTest {

    @Mock
    private StatementManager statementManager;

    @Mock
    private Transaction transaction;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Database database;

    @Mock
    private Table table;

    @Mock
    private List<Entry> entries;

    @Mock
    private DropTableWrapper dropTableStatement;

    @InjectMocks
    private DropTableService dropTableService;

    @Test
    void testDropTable_normally() throws SQLException {
        when(statementManager.getDatabase()).thenReturn(database);
        when(statementManager.getTableByName(dropTableStatement.getTableName())).thenReturn(table);
        when(dropTableStatement.isIfExistsPresent()).thenReturn(false);
        when(semanticValidator.tableExists(dropTableStatement, database)).thenReturn(true);

        when(table.getEntries()).thenReturn(entries);
        when(table.getEntries().size()).thenReturn(2);

        dropTableService.dropTable(dropTableStatement);

        verify(transaction, times(1)).executeDropTableOperation();

    }

    @Test
    void testDropTable_tableFileNotExists() throws SQLException {
        when(statementManager.getTableByName(dropTableStatement.getTableName())).thenReturn(table);
        when(dropTableStatement.isIfExistsPresent()).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class,
                () -> dropTableService.dropTable(dropTableStatement));
        assertEquals("Cannot DROP " + dropTableStatement.getTableName() + " because the table's file or schema doesn't exist.", thrown.getMessage());

        verify(transaction, never()).executeDropTableOperation();

    }

    @Test
    void testDropTable_ifExists_doesNotThrowException() throws SQLException {
        when(dropTableStatement.isIfExistsPresent()).thenReturn(true);
        when(dropTableStatement.getTableName()).thenReturn("myTable");
        when(statementManager.getTableByName(dropTableStatement.getTableName())).thenThrow(SQLException.class);

        assertDoesNotThrow(() -> dropTableService.dropTable(dropTableStatement));
        verify(transaction, never()).executeDropTableOperation();
    }

}
