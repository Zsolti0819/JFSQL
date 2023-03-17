package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DeleteWrapper;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DeleteServiceTest {

    @Mock
    private TableFinder tableFinder;

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private WhereConditionSolver whereConditionSolver;

    @Mock
    private Table table;

    @Mock
    private List<Entry> entries;

    @Mock
    private List<Entry> whereEntries;

    @Mock
    private DeleteWrapper deleteStatement;

    @InjectMocks
    private DeleteService deleteService;

    @Test
    void testDelete_normally() throws SQLException {
        when(deleteStatement.getWhereColumns()).thenReturn(List.of("column1", "column2", "column3"));
        when(tableFinder.getTableByName(deleteStatement.getTableName())).thenReturn(table);
        when(table.getEntries()).thenReturn(entries);
        when(semanticValidator.allWhereColumnsExist(table, deleteStatement)).thenReturn(true);
        when(whereConditionSolver.getWhereEntries(table, deleteStatement)).thenReturn(whereEntries);

        deleteService.deleteFromTable(deleteStatement);

        verify(entries, times(1)).removeAll(whereEntries);
        verify(transactionManager, times(1)).executeDMLOperation(table);
    }

    @Test
    void testDelete_whereColumnIsEmpty() throws SQLException {
        when(deleteStatement.getWhereColumns()).thenReturn(Collections.emptyList());
        when(tableFinder.getTableByName(deleteStatement.getTableName())).thenReturn(table);
        when(table.getEntries()).thenReturn(entries);

        deleteService.deleteFromTable(deleteStatement);

        verify(entries, times(1)).clear();
        verify(transactionManager, times(1)).executeDMLOperation(table);
    }

    @Test
    void testDelete_columnsNotExists() throws SQLException {
        when(tableFinder.getTableByName(deleteStatement.getTableName())).thenReturn(table);
        when(deleteStatement.getWhereColumns()).thenReturn(List.of("column1", "column2", "column3"));
        when(semanticValidator.allWhereColumnsExist(table, deleteStatement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> deleteService.deleteFromTable(deleteStatement));
        assertEquals("Some columns entered doesn't exist in \"" + table.getName() + "\".", thrown.getMessage());

        verify(transactionManager, never()).executeDMLOperation(table);
        verifyNoInteractions(whereConditionSolver);
        verifyNoInteractions(entries);
    }
}