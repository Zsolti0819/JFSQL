package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.DeleteStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DeleteServiceTest {

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private WhereConditionSolver whereConditionSolver;

    @Mock
    private Table table;

    @Mock
    private Database database;

    @Mock
    private DeleteStatement statement;

    @InjectMocks
    private DeleteService deleteService;

    @Test
    void testDelete_normally() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getWhereColumns()).thenReturn(List.of("column1"));
        when(statement.getWhereValues()).thenReturn(List.of("1"));
        when(statement.getSymbols()).thenReturn(List.of("="));
        when(table.getColumnsAndTypes()).thenReturn(Map.of("column1", "INTEGER"));
        final List<Entry> entries = new ArrayList<>();
        entries.add(new Entry(Map.of("column1", "1"), Collections.emptyMap()));
        when(table.getEntries()).thenReturn(entries);
        when(semanticValidator.allWhereColumnsExist(table, statement)).thenReturn(true);

        deleteService.deleteFromTable(statement);

        verify(transactionManager, times(1)).execute(any(), any(), any());
    }

    @Test
    void testDelete_columnsNotExists() throws SQLException {
        final List<Table> tables = new ArrayList<>();
        tables.add(table);
        when(database.getTables()).thenReturn(tables);
        when(table.getName()).thenReturn("myTable");
        when(statement.getTableName()).thenReturn("myTable");
        when(statement.getWhereColumns()).thenReturn(List.of("column1", "column2", "column3"));
        when(semanticValidator.allWhereColumnsExist(table, statement)).thenReturn(false);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> deleteService.deleteFromTable(statement));
        assertEquals("Some columns entered doesn't exist in '" + table.getName() + "'.", thrown.getMessage());

        verify(transactionManager, never()).execute(any(), any(), any());
        verifyNoInteractions(whereConditionSolver);
    }
}
