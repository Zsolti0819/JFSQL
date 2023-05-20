package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DropTableStatement;
import com.github.jfsql.parser.dto.DropTableWrapper;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DropTableServiceTest {

    @Mock
    private Database database;

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Table table;

    @Mock
    private List<Entry> entries;

    @Mock
    private DropTableStatement statement;

    @InjectMocks
    private DropTableService dropTableService;

    @Test
    void testDropTable_normally() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.isIfExistsPresent()).thenReturn(false);
        when(semanticValidator.tableExists(statement, database)).thenReturn(true);

        when(table.getEntries()).thenReturn(entries);
        when(table.getEntries().size()).thenReturn(2);

        dropTableService.dropTable(statement);

        verify(transactionManager, times(1)).execute(any(), any(), any());

    }

    @Test
    void testDropTable_tableFileNotExists() throws SQLException {
        when(database.getTables()).thenReturn(List.of(table));
        when(statement.isIfExistsPresent()).thenReturn(false);
        final SQLException thrown = assertThrows(SQLException.class,
            () -> dropTableService.dropTable(statement));
        assertEquals(
            "Cannot DROP " + statement.getTableName() + " because the table's file or schema doesn't exist.",
            thrown.getMessage());

        verify(transactionManager, never()).execute(any(), any(), any());

    }

    @Test
    void testDropTable_ifExists_doesNotThrowException() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        when(database.getTables()).thenReturn(tables);
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.isIfExistsPresent()).thenReturn(true);

        assertDoesNotThrow(() -> dropTableService.dropTable(statement));
        verify(transactionManager, never()).execute(any(), any(), any());
    }

}
