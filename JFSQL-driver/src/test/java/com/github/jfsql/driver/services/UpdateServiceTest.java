package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class UpdateServiceTest {

    @Mock
    private TableFinder tableFinder;

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private ColumnToTypeMapper columnToTypeMapper;

    @Mock
    private WhereConditionSolver whereConditionSolver;

    @Mock
    private Table table;

    @Mock
    private UpdateWrapper statement;

    @InjectMocks
    private UpdateService updateService;

    @Test
    void testUpdate_normally() throws SQLException {
        when(statement.getColumns()).thenReturn(List.of("column1"));
        when(statement.getValues()).thenReturn(List.of("1"));
        when(tableFinder.getTableByName(statement.getTableName())).thenReturn(table);
        when(semanticValidator.allColumnsExist(table, statement)).thenReturn(true);
        when(semanticValidator.allWhereColumnsExist(table, statement)).thenReturn(true);
        when(columnToTypeMapper.mapColumnsToTypes(statement, table)).thenReturn(Map.of("column1", "INTEGER"));
        when(whereConditionSolver.getWhereEntries(table, statement)).thenReturn(Collections.emptyList());

        updateService.updateTable(statement);

        verify(transactionManager, times(1)).executeOperation(table, false);
    }

    @Test
    void testUpdate_columnsNotExists() throws SQLException {
        when(table.getName()).thenReturn("myTable");
        when(tableFinder.getTableByName(statement.getTableName())).thenReturn(table);
        when(semanticValidator.allColumnsExist(table, statement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class, () -> updateService.updateTable(
            statement));
        assertEquals("Some columns entered doesn't exist in '" + table.getName() + "'.", thrown.getMessage());

        verifyNoInteractions(whereConditionSolver);
        verifyNoInteractions(columnToTypeMapper);
        verify(transactionManager, never()).executeOperation(table, false);
    }

    @Test
    void testUpdate_whereColumnsNotExist() throws SQLException {
        when(table.getName()).thenReturn("myTable");
        when(tableFinder.getTableByName(statement.getTableName())).thenReturn(table);
        when(semanticValidator.allColumnsExist(table, statement)).thenReturn(true);
        when(semanticValidator.allWhereColumnsExist(table, statement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class, () -> updateService.updateTable(
            statement));
        assertEquals("Some columns entered doesn't exist in '" + table.getName() + "'.", thrown.getMessage());

        verifyNoInteractions(whereConditionSolver);
        verifyNoInteractions(columnToTypeMapper);
        verify(transactionManager, never()).executeOperation(table, false);
    }
}
