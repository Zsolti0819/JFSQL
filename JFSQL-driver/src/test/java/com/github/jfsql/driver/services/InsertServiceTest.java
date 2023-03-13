package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.InsertWrapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.SQLException;
import java.util.List;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class InsertServiceTest {

    @Mock
    private TableFinder tableFinder;

    @Mock
    private TransactionManager transactionManager;

    @Mock
    private SemanticValidator semanticValidator;

    @Mock
    private Table table;

    @Mock
    private List<Entry> entries;

    @Mock
    private InsertWrapper insertStatement;

    @InjectMocks
    private InsertService insertService;

    @Test
    void testInsert_noExplicitColumns() throws SQLException {
        when(semanticValidator.allInsertValuesAreEqualLength(insertStatement)).thenReturn(true);
        when(tableFinder.getTableByName(insertStatement.getTableName())).thenReturn(table);
        when(semanticValidator.valueCountIsEqualToTableColumnCount(table, insertStatement)).thenReturn(true);
        when(semanticValidator.allColumnsExist(table, insertStatement)).thenReturn(true);
        when(semanticValidator.allInsertValuesAreValid(table, insertStatement)).thenReturn(true);
        when(table.getEntries()).thenReturn(entries);

        insertService.insertIntoTable(insertStatement);

        verify(entries, times(1)).addAll(any());
        verify(transactionManager, times(1)).executeDMLOperation(table);
    }
}
