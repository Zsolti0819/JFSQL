package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.StatementWithColumns;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ColumnToTypeMapper {

    /**
     * @param statement The statement from which the columns will be extracted
     * @param table     The table in which all the columns are paired to a type
     * @return Ordered map in which the column names are paired to types. This map can but don't have to contain less
     * key-value pairs as the table's original columnsAndTypes map.
     */
    public Map<String, String> mapColumnsToTypes(final StatementWithColumns statement, final Table table) {
        final List<String> statementColumnNames = new ArrayList<>(statement.getColumns());
        final Map<String, String> columnsAndTypes = new LinkedHashMap<>(table.getSchema().getColumnsAndTypes());
        final List<String> excludedColumnNames = getExcludedColumnNames(statementColumnNames,
            new ArrayList<>(columnsAndTypes.keySet()));

        // Remove from the map those pairs, which we don't want to select
        for (final String columnName : excludedColumnNames) {
            columnsAndTypes.remove(columnName);
        }

        final Map<String, String> orderedColumnsAndTypes = new LinkedHashMap<>();
        final String[] columns = columnsAndTypes.keySet().toArray(new String[0]);
        final String[] types = columnsAndTypes.values().toArray(new String[0]);

        for (int i = 0; i < columnsAndTypes.size(); i++) {
            for (int j = 0; j < columns.length; j++) {
                if (Objects.equals(statementColumnNames.get(i), columns[j])) {
                    orderedColumnsAndTypes.put(columns[j], types[j]);
                }
            }
        }
        return orderedColumnsAndTypes;
    }

    private List<String> getExcludedColumnNames(final List<String> selectedColumnNames,
        final List<String> tableColumns) {
        final List<String> excludedColumnNames = new ArrayList<>(tableColumns);
        excludedColumnNames.removeAll(selectedColumnNames);
        return excludedColumnNames;
    }
}