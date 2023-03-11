package com.github.jfsql.driver.core;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.statements.StatementManager;
import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.*;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class JfsqlStatement implements Statement {

    private final Parser parser;
    private final StatementManager statementManager;
    private final Cache cache;
    private Connection connection;
    private ResultSet resultSet;
    private final List<String> batch;

    JfsqlStatement(final Connection connection, final StatementManager statementManager, final Cache cache) {
        this.connection = connection;
        this.statementManager = statementManager;
        this.cache = cache;
        batch = new ArrayList<>();
        parser = new Parser();
    }

    @Override
    public boolean execute(final String sql) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        if (TypeOfStatement.SELECT.equals(statement.getTypeOfStatement())) {
            executeQuery(statement);
            return true;
        } else {
            executeUpdate(statement);
            return false;
        }
    }

    private void executeQuery(final BaseStatement statement) throws SQLException {
        resultSet = statementManager.selectFromTable((SelectWrapper) statement);
    }

    private void executeUpdate(final BaseStatement statement) throws SQLException {
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
                statementManager.alterTable((AlterTableWrapper) statement);
                break;
            case CREATE_DATABASE:
                statementManager.createDatabase((CreateDatabaseWrapper) statement);
                break;
            case CREATE_TABLE:
                statementManager.createTable((CreateTableWrapper) statement);
                break;
            case DELETE:
                statementManager.deleteFromTable((DeleteWrapper) statement);
                break;
            case DROP_DATABASE:
                statementManager.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                statementManager.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                statementManager.insertIntoTable((InsertWrapper) statement);
                break;
            case UPDATE:
                statementManager.updateTable((UpdateWrapper) statement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
    }

    @Override
    public ResultSet executeQuery(final String sql) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        if (!(TypeOfStatement.SELECT.equals(statement.getTypeOfStatement()))) {
            throw new SQLException("Cannot execute executeQuery() because statement was not a Select statement.");
        }
        return statementManager.selectFromTable((SelectWrapper) statement);
    }

    @Override
    public int executeUpdate(final String arg0) throws SQLException {
        int updateCount = 0;
        final BaseStatement statement = getFromCacheOrParseStatement(arg0);
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
                statementManager.alterTable((AlterTableWrapper) statement);
                break;
            case CREATE_DATABASE:
                statementManager.createDatabase((CreateDatabaseWrapper) statement);
                break;
            case CREATE_TABLE:
                statementManager.createTable((CreateTableWrapper) statement);
                break;
            case DELETE:
                updateCount = statementManager.deleteFromTable((DeleteWrapper) statement);
                break;
            case DROP_DATABASE:
                updateCount = statementManager.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                updateCount = statementManager.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                updateCount = statementManager.insertIntoTable((InsertWrapper) statement);
                break;
            case SELECT:
                resultSet = statementManager.selectFromTable((SelectWrapper) statement);
                break;
            case UPDATE:
                updateCount = statementManager.updateTable((UpdateWrapper) statement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
        return updateCount;
    }

    private BaseStatement getFromCacheOrParseStatement(final String sql) {
        final BaseStatement statement;
        if (cache.getCachedStatements().containsKey(sql)) {
            statement = cache.getCachedStatements().get(sql);
        } else {
            statement = parser.parse(sql);
            cache.getCachedStatements().put(sql, statement);
        }
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        return statement;
    }

    @Override
    public Connection getConnection() {
        return connection;
    }

    @Override
    public ResultSet getResultSet() {
        return resultSet;
    }

    @Override
    public void addBatch(final String sql) {
        batch.add(sql);
    }

    @Override
    public void clearBatch() {
        batch.clear();
    }

    @Override
    public int[] executeBatch() throws SQLException {
        connection.setAutoCommit(false);
        final int[] batchResults = new int[batch.size()];
        for (int i = 0; i < batch.size(); i++) {
            batchResults[i] = executeUpdate(batch.get(i));
        }
        connection.commit();
        connection.setAutoCommit(true);
        return batchResults;
    }

    @Override
    public void close() {
        connection = null;
    }

    // Unsupported operations

    @Override
    public boolean isWrapperFor(final Class<?> arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public <T> T unwrap(final Class<T> arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void cancel() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void clearWarnings() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getFetchDirection() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setFetchDirection(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getFetchSize() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setFetchSize(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public ResultSet getGeneratedKeys() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getMaxFieldSize() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setMaxFieldSize(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getMaxRows() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setMaxRows(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean getMoreResults() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean getMoreResults(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getQueryTimeout() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setQueryTimeout(final int seconds) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getResultSetConcurrency() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getResultSetHoldability() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getResultSetType() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getUpdateCount() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public SQLWarning getWarnings() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isClosed() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isPoolable() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setPoolable(final boolean arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setCursorName(final String arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setEscapeProcessing(final boolean arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void closeOnCompletion() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isCloseOnCompletion() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean execute(final String arg0, final int arg1) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean execute(final String arg0, final int[] arg1) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean execute(final String arg0, final String[] arg1) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int executeUpdate(final String arg0, final int arg1) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int executeUpdate(final String arg0, final int[] arg1) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int executeUpdate(final String arg0, final String[] arg1) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

}
