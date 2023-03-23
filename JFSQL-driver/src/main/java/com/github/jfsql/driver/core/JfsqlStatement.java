package com.github.jfsql.driver.core;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.services.StatementServiceManager;
import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import com.github.jfsql.parser.dto.BaseStatement;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import com.github.jfsql.parser.dto.DeleteWrapper;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import com.github.jfsql.parser.dto.DropTableWrapper;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.SelectWrapper;
import com.github.jfsql.parser.dto.TypeOfStatement;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.SQLWarning;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class JfsqlStatement implements Statement {

    private final Parser parser;
    private final StatementServiceManager statementServiceManager;
    private final Cache cache;
    private final List<String> batch;
    private Connection connection;
    private ResultSet resultSet;

    JfsqlStatement(final Connection connection, final StatementServiceManager statementServiceManager,
        final Cache cache) {
        this.connection = connection;
        this.statementServiceManager = statementServiceManager;
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
        resultSet = statementServiceManager.selectFromTable((SelectWrapper) statement);
    }

    private void executeUpdate(final BaseStatement statement) throws SQLException {
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
                statementServiceManager.alterTable((AlterTableWrapper) statement);
                break;
            case CREATE_DATABASE:
                statementServiceManager.createDatabase((CreateDatabaseWrapper) statement);
                break;
            case CREATE_TABLE:
                statementServiceManager.createTable((CreateTableWrapper) statement);
                break;
            case DELETE:
                statementServiceManager.deleteFromTable((DeleteWrapper) statement);
                break;
            case DROP_DATABASE:
                statementServiceManager.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                statementServiceManager.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                statementServiceManager.insertIntoTable((InsertWrapper) statement);
                break;
            case UPDATE:
                statementServiceManager.updateTable((UpdateWrapper) statement);
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
        return statementServiceManager.selectFromTable((SelectWrapper) statement);
    }

    @Override
    public int executeUpdate(final String arg0) throws SQLException {
        int updateCount = 0;
        final BaseStatement statement = getFromCacheOrParseStatement(arg0);
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
                updateCount = statementServiceManager.alterTable((AlterTableWrapper) statement);
                break;
            case CREATE_DATABASE:
                updateCount = statementServiceManager.createDatabase((CreateDatabaseWrapper) statement);
                break;
            case CREATE_TABLE:
                updateCount = statementServiceManager.createTable((CreateTableWrapper) statement);
                break;
            case DELETE:
                updateCount = statementServiceManager.deleteFromTable((DeleteWrapper) statement);
                break;
            case DROP_DATABASE:
                updateCount = statementServiceManager.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                updateCount = statementServiceManager.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                updateCount = statementServiceManager.insertIntoTable((InsertWrapper) statement);
                break;
            case SELECT:
                resultSet = statementServiceManager.selectFromTable((SelectWrapper) statement);
                break;
            case UPDATE:
                updateCount = statementServiceManager.updateTable((UpdateWrapper) statement);
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
