package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.StatementWithColumns;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ColumnToTypeMapper {

    public Map<String, String> mapColumnsToTypes(final StatementWithColumns statement, final Table table) {
        final List<String> statementColumnNames = new ArrayList<>(statement.getColumns());
        final Map<String, String> columnsAndTypes = new LinkedHashMap<>(table.getColumnsAndTypes());
        final Map<String, String> orderedColumnsAndTypes = new LinkedHashMap<>();

        for (final String column : statementColumnNames) {
            if (columnsAndTypes.containsKey(column)) {
                orderedColumnsAndTypes.put(column, columnsAndTypes.get(column));
            } else {
                final List<String> matchingColumns = new ArrayList<>();
                for (final String tableColumn : columnsAndTypes.keySet()) {
                    if (!column.contains(".") && tableColumn.endsWith(column)) {
                        matchingColumns.add(tableColumn);
                    }
                }
                if (matchingColumns.size() > 1) {
                    throw new IllegalStateException(
                        "Multiple columns match the given column names: " + matchingColumns);
                } else if (!matchingColumns.isEmpty()) {
                    orderedColumnsAndTypes.put(matchingColumns.get(0), columnsAndTypes.get(matchingColumns.get(0)));
                }
            }
        }

        return orderedColumnsAndTypes;
    }

}
