package com.github.jfsql.driver.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.StatementWithColumns;
import com.github.jfsql.parser.dto.UpdateStatement;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class ColumnToTypeMapperTest {

    private ColumnToTypeMapper columnToTypeMapper;

    @BeforeEach
    public void setUp() {
        columnToTypeMapper = new ColumnToTypeMapper();
    }

    @Test
    public void mapColumnsToTypes() {
        final StatementWithColumns statement = new UpdateStatement("myTable", List.of("age", "id", "name"),
            List.of("25", "1", "Zsolti"),
            Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());

        final Table table = Table.builder()
            .columnsAndTypes(Map.of("id", "INTEGER", "name", "TEXT", "age", "INTEGER"))
            .build();

        final Map<String, String> expectedMap = new LinkedHashMap<>();
        expectedMap.put("age", "INTEGER");
        expectedMap.put("id", "INTEGER");
        expectedMap.put("name", "TEXT");

        final Map<String, String> columnsMappedToTypes = columnToTypeMapper.mapColumnsToTypes(statement, table);

        assertEquals(expectedMap, columnsMappedToTypes);
    }
}