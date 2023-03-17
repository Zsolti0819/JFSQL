package com.github.jfsql.driver.util;

import com.github.jfsql.driver.core.JfsqlPreparedStatement;
import com.github.jfsql.parser.dto.DeleteStatement;
import com.github.jfsql.parser.dto.DeleteWrapper;
import com.github.jfsql.parser.dto.InsertStatement;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.JoinType;
import com.github.jfsql.parser.dto.SelectStatement;
import com.github.jfsql.parser.dto.SelectWrapper;
import com.github.jfsql.parser.dto.UpdateStatement;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class PreparedStatementCreator {

    public DeleteWrapper getPreparedDeleteStatement(final DeleteWrapper statement,
        final JfsqlPreparedStatement preparedStatement) {
        final String tableName = statement.getTableName();
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = replacePlaceholders(preparedStatement, whereColumns,
            statement.getWhereValues(), 0);
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();
        return new DeleteStatement(tableName, whereColumns, whereValues, symbols, binaryOperators);
    }

    public InsertWrapper getPreparedInsertStatement(final InsertWrapper statement,
        final JfsqlPreparedStatement preparedStatement) throws SQLException {
        final String tableName = statement.getTableName();
        final List<String> columns;
        final List<List<String>> listOfValueLists = new ArrayList<>();
        if (statement.getColumns().isEmpty()) {
            columns = List.of(preparedStatement.getTableFinder().getTableByName(statement.getTableName()).getColumns());
        } else {
            columns = statement.getColumns();
        }
        for (int i = 0; i < statement.getValues().size(); i++) {
            final List<String> values = replacePlaceholders(preparedStatement, columns,
                new ArrayList<>(statement.getValues().get(i)), 0);
            listOfValueLists.add(values);
        }
        return new InsertStatement(tableName, columns, listOfValueLists);
    }

    public SelectWrapper getPreparedSelectStatement(final SelectWrapper statement,
        final JfsqlPreparedStatement preparedStatement) {
        final String tableName = statement.getTableName();
        final List<String> joinTableNames = statement.getJoinTableNames();
        final List<JoinType> joinTypes = statement.getJoinTypes();
        final List<String> columns = statement.getColumns();
        final List<List<String>> listOfJoinColumns = statement.getListOfJoinColumns();
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = replacePlaceholders(preparedStatement, whereColumns,
            statement.getWhereValues(), 0);
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();
        return new SelectStatement(tableName, joinTableNames, joinTypes, columns, listOfJoinColumns, whereColumns,
            whereValues, symbols, binaryOperators);
    }

    public UpdateWrapper getPreparedUpdateStatement(final UpdateWrapper statement,
        final JfsqlPreparedStatement preparedStatement) {
        final String tableName = statement.getTableName();
        final List<String> columns = statement.getColumns();
        final List<String> values = replacePlaceholders(preparedStatement, columns, statement.getWhereValues(), 0);
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = replacePlaceholders(preparedStatement, whereColumns,
            statement.getWhereValues(), columns.size());
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();
        return new UpdateStatement(tableName, columns, values, whereColumns, whereValues, symbols, binaryOperators);
    }

    private List<String> replacePlaceholders(final JfsqlPreparedStatement preparedStatement, final List<String> columns,
        final List<String> values, final int offset) {
        final List<String> modifiedList = new ArrayList<>();
        for (int i = 0; i < columns.size(); i++) {
            if (Objects.equals(values.get(i), "?")) {
                modifiedList.add(i, String.valueOf(preparedStatement.getParameters()[i + offset]));
            } else {
                modifiedList.add(i, values.get(i + offset));
            }
        }
        return modifiedList;
    }
}
