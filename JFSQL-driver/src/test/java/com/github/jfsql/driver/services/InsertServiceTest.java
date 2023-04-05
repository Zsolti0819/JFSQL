package com.github.jfsql.driver.services;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.PreparedStatementCreator;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.InsertWrapper;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@SuppressWarnings("unused")
@ExtendWith(MockitoExtension.class)
class InsertServiceTest {

    @Mock
    private TableFinder tableFinder;
    @Mock
    private TransactionManager transactionManager;
    @Mock
    private SemanticValidator semanticValidator;
    @Mock
    private PreparedStatementCreator preparedStatementCreator;
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
        when(semanticValidator.valueCountIsLteTableColumnCount(table, statement)).thenReturn(true);
        when(semanticValidator.allColumnsExist(table, statement)).thenReturn(true);
        when(semanticValidator.allInsertValuesAreValid(table, statement)).thenReturn(true);
        when(table.getEntries()).thenReturn(entries);

        insertService.insertIntoTable(statement);

        verify(transactionManager, times(1)).executeDMLOperation(table);
    }

    @Test
    void testGetEntriesToInsert() throws SQLException {
        when(statement.getColumns()).thenReturn(List.of("id", "name", "age", "resumee"));
        when(table.getColumnsAndTypes()).thenReturn(
            Map.of("id", "INTEGER", "name", "TEXT", "age", "INTEGER", "resumee", "BLOB"));
        when(statement.getValues()).thenReturn(List.of(List.of("1", "Zsolti", "25", "null")));
        when(semanticValidator.nullInsertIntoNotNullColumn(anyString(), anyString(), any())).thenReturn(false);
        insertService.getEntryToInsert(statement.getColumns(), statement.getValues().get(0), table);
        when(semanticValidator.nullInsertIntoNotNullColumn(anyString(), anyString(), eq(table)))
            .thenReturn(false);

        final Map<String, String> columnsAndValues = new LinkedHashMap<>();
        columnsAndValues.put("id", "1");
        columnsAndValues.put("name", "Zsolti");
        columnsAndValues.put("age", "25");
        columnsAndValues.put("resumee", "null");
        final Entry expectedEntry = new Entry(columnsAndValues, new HashMap<>());
        final Entry actualEntry = insertService.getEntryToInsert(statement.getColumns(), statement.getValues().get(0),
            table);

        assertEquals(expectedEntry, actualEntry);
    }
}
