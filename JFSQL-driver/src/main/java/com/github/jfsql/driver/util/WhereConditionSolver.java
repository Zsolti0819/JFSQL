package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.parser.dto.StatementWithWhere;
import java.util.ArrayList;
import java.util.List;
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
            if (entry.getColumnsAndValues().containsKey(whereColumn)) {
                final String value = entry.getColumnsAndValues().get(whereColumn);
                if (Objects.equals(symbol, "LIKE") && likeCompare(value, whereValue,
                    table.getColumnsAndTypes().get(whereColumn)) ||
                    !Objects.equals(symbol, "LIKE") && compareValues(value, whereValue, symbol,
                        table.getColumnsAndTypes().get(whereColumn))) {
                    entriesFulfillingConditions.add(entry);
                }
            }
        }
        return entriesFulfillingConditions;
    }

    /**
     * @param originalValue The entry's original value in the database
     * @param whereValue    The value in the where expression
     * @param compareSymbol The symbol in the where expression
     * @param originalType  The original value's type
     * @return The value 0 if originalValue == whereValue; a value less than 0 if originalValue < whereValue; and a
     * value greater than 0 if originalValue > whereValue
     */
    private boolean compareValues(final String originalValue, final String whereValue, final String compareSymbol,
        final String originalType) {
        if (originalValue == null) {
            return false;
        }
        final String javaType = DatatypeConverter.convertFromSqlToJava(originalType);
        final int comparisonResult;
        switch (javaType) {
            case "Long":
                comparisonResult = Long.compare(Long.parseLong(originalValue), Long.parseLong(whereValue));
                break;
            case "Double":
                comparisonResult = Double.compare(Double.parseDouble(originalValue), Double.parseDouble(whereValue));
                break;
            case "String":
                comparisonResult = String.CASE_INSENSITIVE_ORDER.compare(originalValue, whereValue);
                break;
            default:
                throw new IllegalStateException("Cannot compare " + javaType + "datatype.");
        }

        return (Objects.equals("=", compareSymbol) && comparisonResult == 0)
            || (Objects.equals(">", compareSymbol) && comparisonResult > 0)
            || (Objects.equals(">=", compareSymbol) && comparisonResult > 0
            || compareSymbol.equals(">=") && comparisonResult == 0)
            || (Objects.equals("<", compareSymbol) && comparisonResult < 0)
            || (Objects.equals("<=", compareSymbol) && comparisonResult < 0
            || compareSymbol.equals("<=") && comparisonResult == 0);
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
