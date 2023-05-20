package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.parser.dto.BaseStatement;
import com.github.jfsql.parser.dto.DeleteStatement;
import com.github.jfsql.parser.dto.InsertStatement;
import com.github.jfsql.parser.dto.SelectStatement;
import com.github.jfsql.parser.dto.UpdateStatement;
import com.github.jfsql.parser.enums.JoinType;
import com.github.jfsql.parser.enums.OrderBy;
import com.github.jfsql.parser.enums.TypeOfStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class PreparedStatementCreator {

    private final Database database;
    @Getter
    private Object[] parameters;

    public void initParameterCount(final BaseStatement statement) throws SQLException {
        final int parameterCount;
        final TypeOfStatement statementType = statement.getTypeOfStatement();
        switch (statementType) {
            case ALTER_TABLE:
            case CREATE_TABLE:
            case DROP_TABLE:
                parameterCount = 0;
                break;
            case DELETE:
                parameterCount = ((DeleteStatement) statement).getWhereValues().size();
                break;
            case INSERT:
                parameterCount = ((InsertStatement) statement).getValues().get(0).size();
                break;
            case SELECT:
                parameterCount = ((SelectStatement) statement).getWhereValues().size();
                break;
            case UPDATE:
                parameterCount = ((UpdateStatement) statement).getValues().size()
                    + ((UpdateStatement) statement).getWhereValues().size();
                break;
            default:
                throw new SQLException("Cannot determine the type of the statement.");
        }
        parameters = new Object[parameterCount];
    }

    public DeleteStatement getPreparedDeleteStatement(final DeleteStatement statement) {
        final String tableName = statement.getTableName();
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = replaceQuestionmarks(whereColumns, statement.getWhereValues(), 0);
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();

        return DeleteStatement.builder()
            .tableName(tableName)
            .whereColumns(whereColumns)
            .whereValues(whereValues)
            .symbols(symbols)
            .binaryOperators(binaryOperators)
            .build();
    }

    public InsertStatement getPreparedInsertStatement(final InsertStatement statement) throws SQLException {
        final String tableName = statement.getTableName();
        final List<String> columns;
        final List<List<String>> listOfValueLists = new ArrayList<>();
        final List<String> statementColumns = statement.getColumns();
        if (statementColumns.isEmpty()) {
            final Table table = TableFinder.getTableByName(statement.getTableName(), database);
            columns = new ArrayList<>(table.getColumnsAndTypes().keySet());
        } else {
            columns = statementColumns;
        }
        for (int i = 0; i < statement.getValues().size(); i++) {
            final List<String> values = replaceQuestionmarks(columns, new ArrayList<>(statement.getValues().get(i)), 0);
            listOfValueLists.add(values);
        }

        return InsertStatement.builder()
            .tableName(tableName)
            .columns(columns)
            .values(listOfValueLists)
            .build();
    }

    public SelectStatement getPreparedSelectStatement(final SelectStatement statement) {
        final String tableName = statement.getTableName();
        final List<String> joinTableNames = statement.getJoinTableNames();
        final List<JoinType> joinTypes = statement.getJoinTypes();
        final List<String> columns = statement.getColumns();
        final List<List<String>> listOfJoinColumns = statement.getListOfJoinColumns();
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = replaceQuestionmarks(whereColumns, statement.getWhereValues(), 0);
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();
        final String orderColumn = statement.getOrderColumn();
        final OrderBy orderBy = statement.getOrderBy();
        final String limit = statement.getLimit();
        final String offset = statement.getOffset();
        return SelectStatement.builder()
            .tableName(tableName)
            .joinTableNames(joinTableNames)
            .joinTypes(joinTypes)
            .columns(Collections.unmodifiableList(columns))
            .listOfJoinColumns(Collections.unmodifiableList(listOfJoinColumns))
            .whereColumns(whereColumns)
            .whereValues(whereValues)
            .symbols(symbols)
            .binaryOperators(binaryOperators)
            .orderColumn(orderColumn)
            .orderBy(orderBy)
            .limit(limit)
            .offset(offset)
            .build();
    }

    public UpdateStatement getPreparedUpdateStatement(final UpdateStatement statement) {
        final String tableName = statement.getTableName();
        final List<String> columns = statement.getColumns();
        final List<String> values = replaceQuestionmarks(columns, statement.getValues(), 0);
        final List<String> whereColumns = statement.getWhereColumns();
        final List<String> whereValues = replaceQuestionmarks(whereColumns, statement.getWhereValues(), columns.size());
        final List<String> symbols = statement.getSymbols();
        final List<String> binaryOperators = statement.getBinaryOperators();

        return UpdateStatement.builder()
            .tableName(tableName)
            .columns(columns)
            .values(values)
            .whereColumns(whereColumns)
            .whereValues(whereValues)
            .symbols(symbols)
            .binaryOperators(binaryOperators)
            .build();
    }

    private List<String> replaceQuestionmarks(final List<String> columns, final List<String> values, final int offset) {
        final List<String> modifiedList = new ArrayList<>();
        for (int i = 0; i < columns.size(); i++) {
            if (Objects.equals(values.get(i), "?")) {
                final Object parameter = parameters[i + offset];
                if (parameter instanceof LargeObject) {
                    modifiedList.add(i, String.valueOf(((LargeObject) parameter).getURL()));
                } else {
                    modifiedList.add(i, String.valueOf(parameter));
                }
            } else {
                modifiedList.add(i, values.get(i + offset));
            }
        }
        return modifiedList;
    }

    public LargeObject getBlob(final Entry entry, final String column) {
        if (parameters == null) {
            return null;
        }

        for (final Object o : parameters) {
            if (o instanceof LargeObject) {
                final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
                final String blobPath = columnsAndValues.get(column);
                if (Objects.equals(((LargeObject) o).getURL(), blobPath)) {
                    return (LargeObject) o;
                }
            }
        }
        return null;
    }

}
