package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.statement.Cache;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.AlterTableStatement;
import com.github.jfsql.parser.dto.BaseStatement;
import com.github.jfsql.parser.dto.CreateTableStatement;
import com.github.jfsql.parser.dto.DeleteStatement;
import com.github.jfsql.parser.dto.DeleteWrapper;
import com.github.jfsql.parser.dto.DropTableStatement;
import com.github.jfsql.parser.dto.InsertStatement;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.SelectStatement;
import com.github.jfsql.parser.dto.UpdateStatement;
import com.github.jfsql.parser.enums.TypeOfStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;
import lombok.Builder;
import lombok.Getter;

@Builder
public class StatementServiceManager {

    private final Cache cache;
    private final Parser parser;
    private final PreparedStatementCreator preparedStatementCreator;
    private final AlterTableService alterTableService;
    private final CreateTableService createTableService;
    private final InsertService insertService;
    private final SelectService selectService;
    private final UpdateService updateService;
    private final DeleteService deleteService;
    private final DropTableService dropTableService;
    private final IoOperationHandler ioOperationHandler;
    private final SemanticValidator semanticValidator;

    @Getter
    private ResultSet resultSet;
    @Getter
    private int updateCount;

    private BaseStatement getFromCacheOrParseStatement(final String sql) {
        final BaseStatement statement;
        final Map<String, BaseStatement> cachedStatements = cache.getCachedStatements();
        if (cachedStatements.containsKey(sql)) {
            statement = cachedStatements.get(sql);
        } else {
            statement = parser.parse(sql);
            cachedStatements.put(sql, statement);
        }
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        return statement;
    }

    public ResultSet executeQuery(final String sql, final boolean isPreparedStatement) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        if (!(TypeOfStatement.SELECT.equals(statement.getTypeOfStatement()))) {
            throw new SQLException("Method not supported for this statement.");
        }
        resultSet = selectService.selectFromTable(isPreparedStatement ?
            preparedStatementCreator.getPreparedSelectStatement((SelectStatement) statement) :
            (SelectStatement) statement);
        return resultSet;
    }

    public int executeUpdate(final String sql, final boolean isPreparedStatement) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        final TypeOfStatement statementType = statement.getTypeOfStatement();
        switch (statementType) {
            case ALTER_TABLE:
                updateCount = alterTableService.alterTable((AlterTableStatement) statement);
                break;
            case CREATE_TABLE:
                updateCount = createTableService.createTable((CreateTableStatement) statement);
                break;
            case DELETE:
                updateCount = deleteService.deleteFromTable((DeleteStatement) (isPreparedStatement ?
                    preparedStatementCreator.getPreparedDeleteStatement((DeleteStatement) statement) :
                    (DeleteWrapper) statement));
                break;
            case DROP_TABLE:
                updateCount = dropTableService.dropTable((DropTableStatement) statement);
                break;
            case INSERT:
                updateCount = insertService.insertIntoTable((InsertStatement) (isPreparedStatement ?
                    preparedStatementCreator.getPreparedInsertStatement((InsertStatement) statement) :
                    (InsertWrapper) statement));
                break;
            case SELECT:
                throw new SQLException("Method not supported.");
            case UPDATE:
                updateCount = updateService.updateTable(isPreparedStatement ?
                    preparedStatementCreator.getPreparedUpdateStatement((UpdateStatement) statement) :
                    (UpdateStatement) statement);
                break;
            default:
                throw new SQLException("Statement type '" + statementType + "' is not supported.");
        }
        return updateCount;
    }

    public boolean execute(final String sql, final boolean isPreparedStatement) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        final TypeOfStatement statementType = statement.getTypeOfStatement();
        switch (statementType) {
            case ALTER_TABLE:
                updateCount = alterTableService.alterTable((AlterTableStatement) statement);
                break;
            case CREATE_TABLE:
                updateCount = createTableService.createTable((CreateTableStatement) statement);
                break;
            case DELETE:
                updateCount = deleteService.deleteFromTable((DeleteStatement) (isPreparedStatement ?
                    preparedStatementCreator.getPreparedDeleteStatement((DeleteStatement) statement) :
                    (DeleteWrapper) statement));
                break;
            case DROP_TABLE:
                updateCount = dropTableService.dropTable((DropTableStatement) statement);
                break;
            case INSERT:
                updateCount = insertService.insertIntoTable((InsertStatement) (isPreparedStatement ?
                    preparedStatementCreator.getPreparedInsertStatement((InsertStatement) statement) :
                    (InsertWrapper) statement));
                break;
            case SELECT:
                resultSet = selectService.selectFromTable(isPreparedStatement ?
                    preparedStatementCreator.getPreparedSelectStatement((SelectStatement) statement) :
                    (SelectStatement) statement);
                return true;
            case UPDATE:
                updateCount = updateService.updateTable(isPreparedStatement ?
                    preparedStatementCreator.getPreparedUpdateStatement((UpdateStatement) statement) :
                    (UpdateStatement) statement);
                break;
            default:
                throw new SQLException("Statement type '" + statementType + "' is not supported.");
        }
        return false;
    }

    public void initParameterCount(final String sql) throws SQLException {
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        preparedStatementCreator.initParameterCount(statement);
    }

    public Object[] getParameters() {
        return preparedStatementCreator.getParameters();
    }

}
