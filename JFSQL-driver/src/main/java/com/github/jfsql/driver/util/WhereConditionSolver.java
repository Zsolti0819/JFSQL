package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.StatementWithWhere;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class WhereConditionSolver {

    public List<Entry> getWhereEntries(final Table table, final StatementWithWhere statement) {
        final List<Entry> entries = table.getEntries();
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = statement.getWhereValues();
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();

        if (whereColumns.isEmpty()) {
            return entries;
        }

        List<Entry> whereEntries = entries;

        if (binaryOperators.isEmpty()) {
            whereEntries = getEntries(entries, table, whereColumns.get(0), whereValues.get(0), symbols.get(0));
            return whereEntries;
        }

        for (int i = 0; i < whereColumns.size(); i++) {
            final String whereColumn = whereColumns.get(i);
            final String whereValue = whereValues.get(i);
            final String symbol = symbols.get(i);
            if (i == 0) {
                whereEntries = getEntries(whereEntries, table, whereColumn, whereValue, symbol);
            } else {
                if (Objects.equals(binaryOperators.get(i - 1), "AND")) {
                    whereEntries = getEntries(whereEntries, table, whereColumn, whereValue, symbol);
                } else if (Objects.equals(binaryOperators.get(i - 1), "OR")) {
                    whereEntries.addAll(getEntries(entries, table, whereColumn, whereValue, symbol));
                }
            }
        }

        return whereEntries;
    }

    private List<Entry> getEntries(final List<Entry> entries, final Table table, final String whereColumn,
        final String whereValue, final String symbol) {
        final List<Entry> entriesFulfillingConditions = new ArrayList<>();
        for (final Entry entry : entries) {
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            if (columnsAndValues.containsKey(whereColumn)) {
                final String value = columnsAndValues.get(whereColumn);
                final String type = table.getColumnsAndTypes().get(whereColumn);
                if (Objects.equals(symbol, "LIKE") && likeCompare(value, whereValue, type) ||
                    !Objects.equals(symbol, "LIKE") && compareValues(value, whereValue, symbol, type)) {
                    entriesFulfillingConditions.add(entry);
                }
            }
        }
        return entriesFulfillingConditions;
    }

    private boolean compareValues(final String originalValue, final String whereValue, final String symbol,
        final String type) {
        if (originalValue == null) {
            return false;
        }
        final String javaType = DatatypeConverter.convertFromSqlToJava(type);
        final int result;
        switch (javaType) {
            case "Long":
                result = Long.compare(Long.parseLong(originalValue), Long.parseLong(whereValue));
                break;
            case "Double":
                result = Double.compare(Double.parseDouble(originalValue), Double.parseDouble(whereValue));
                break;
            case "String":
                result = String.CASE_INSENSITIVE_ORDER.compare(originalValue, whereValue);
                break;
            default:
                throw new IllegalStateException("Cannot compare " + javaType + "datatype.");
        }

        return (Objects.equals("=", symbol) && result == 0)
            || (Objects.equals(">", symbol) && result > 0)
            || (Objects.equals(">=", symbol) && result > 0
            || symbol.equals(">=") && result == 0)
            || (Objects.equals("<", symbol) && result < 0)
            || (Objects.equals("<=", symbol) && result < 0
            || symbol.equals("<=") && result == 0);
    }

    private boolean likeCompare(final String originalValue, final String whereValue, final String originalType) {
        final String javaType = DatatypeConverter.convertFromSqlToJava(originalType);
        if (!Objects.equals(javaType, "String")) {
            throw new IllegalArgumentException("For INTEGER and REAL values use the <,<=,=,>,>= operators.");
        }
        String pattern = whereValue.replace("_", ".");
        pattern = pattern.replace("%", ".*");
        return originalValue.matches(pattern);

    }
}
