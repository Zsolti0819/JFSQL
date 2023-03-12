package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.UpdateWrapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UpdateServiceTest {

    private final List<Entry> whereEntries = List.of(
            new Entry(Map.of("column1", "1", "column2", "a", "column3", "2.5")));
    private final Map<String, String> mappedColumnsAndTypes = Map.of("column1", "INTEGER", "TEXT", "column2", "column3",
            "REAL");
    @Mock
    private StatementManager statementManager;

    @Mock
    private Transaction transaction;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private ColumnToTypeMapper columnToTypeMapper;

    @Mock
    private WhereConditionSolver whereConditionSolver;

    @Mock
    private Table table;

    @Mock
    private UpdateWrapper updateStatement;

    @InjectMocks
    private UpdateService updateService;

    @Test
    void testUpdate_normally() throws SQLException {
        when(updateStatement.getColumns()).thenReturn(List.of("column1", "column2", "column3"));
        when(updateStatement.getValues()).thenReturn(List.of("1", "a", "2.5"));
        when(statementManager.getTableByName(updateStatement.getTableName())).thenReturn(table);
        when(semanticValidator.allColumnsExist(table, updateStatement)).thenReturn(true);
        when(semanticValidator.allWhereColumnsExist(table, updateStatement)).thenReturn(true);
        when(columnToTypeMapper.mapColumnsToTypes(updateStatement, table)).thenReturn(mappedColumnsAndTypes);
        when(whereConditionSolver.getWhereEntries(table, updateStatement)).thenReturn(whereEntries);

        updateService.updateTable(updateStatement);

        verify(transaction, times(1)).executeDMLOperation(table);
    }

    @Test
    void testUpdate_columnsNotExists() throws SQLException {
        when(table.getName()).thenReturn("myTable");
        when(statementManager.getTableByName(updateStatement.getTableName())).thenReturn(table);
        when(semanticValidator.allColumnsExist(table, updateStatement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class,
                () -> updateService.updateTable(updateStatement));
        assertEquals("Some columns entered doesn't exist in \"" + table.getName() + "\".", thrown.getMessage());

        verifyNoInteractions(whereConditionSolver);
        verifyNoInteractions(columnToTypeMapper);
        verify(transaction, never()).executeDMLOperation(table);
    }

    @Test
    void testUpdate_whereColumnsNotExist() throws SQLException {
        when(table.getName()).thenReturn("myTable");
        when(statementManager.getTableByName(updateStatement.getTableName())).thenReturn(table);
        when(semanticValidator.allColumnsExist(table, updateStatement)).thenReturn(true);
        when(semanticValidator.allWhereColumnsExist(table, updateStatement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class,
                () -> updateService.updateTable(updateStatement));
        assertEquals("Some columns entered doesn't exist in \"" + table.getName() + "\".", thrown.getMessage());

        verifyNoInteractions(whereConditionSolver);
        verifyNoInteractions(columnToTypeMapper);
        verify(transaction, never()).executeDMLOperation(table);
    }
}