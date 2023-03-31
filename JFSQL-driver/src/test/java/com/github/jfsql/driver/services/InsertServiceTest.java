package com.github.jfsql.driver.services;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.InsertWrapper;
import java.sql.SQLException;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

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
    private InsertWrapper statement;
    @InjectMocks
    private InsertService insertService;

    @Test
    void testInsert_noExplicitColumns() throws SQLException {
        when(semanticValidator.allInsertValuesAreEqualLength(statement)).thenReturn(true);
        when(tableFinder.getTableByName(statement.getTableName())).thenReturn(table);
        when(semanticValidator.valueCountIsEqualToTableColumnCount(table, statement)).thenReturn(true);
        when(semanticValidator.allColumnsExist(table, statement)).thenReturn(true);
        when(semanticValidator.allInsertValuesAreValid(table, statement)).thenReturn(true);
        when(table.getEntries()).thenReturn(entries);

        insertService.insertIntoTable(statement);

        verify(entries, times(1)).addAll(any());
        verify(transactionManager, times(1)).executeDMLOperation(table);
    }
}
