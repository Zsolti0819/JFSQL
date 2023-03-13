package com.github.jfsql.driver.core;

import com.github.jfsql.driver.services.StatementServiceManager;
import com.github.jfsql.driver.services.TableFinder;
import com.github.jfsql.driver.util.PreparedStatementCreator;
import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.*;
import lombok.Data;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.*;
import java.util.Arrays;
import java.util.Base64;
import java.util.Calendar;

@Data
public class JfsqlPreparedStatement implements PreparedStatement {

    private final Parser parser;
    private final StatementServiceManager statementServiceManager;
    private final TableFinder tableFinder;
    private final String preparedStatement;
    private final Object[] parameters;
    private JfsqlConnection connection;
    private ResultSet resultSet;
    private int updateCount = 0;

    JfsqlPreparedStatement(final JfsqlConnection connection, final String preparedStatement,
                           final TableFinder tableFinder, final StatementServiceManager statementServiceManager) throws SQLException {
        this.connection = connection;
        this.preparedStatement = preparedStatement;
        this.tableFinder = tableFinder;
        this.statementServiceManager = statementServiceManager;
        parser = new Parser();
        parameters = new Object[getParameterCount(preparedStatement)];
    }

    private int getParameterCount(final String sql) throws SQLException {
        final int parameterCount;
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
            case CREATE_DATABASE:
            case CREATE_TABLE:
            case DROP_DATABASE:
            case DROP_TABLE:
                parameterCount = 0;
                break;
            case DELETE:
                parameterCount = ((DeleteWrapper) statement).getWhereValues().size();
                break;
            case INSERT:
                parameterCount = ((InsertWrapper) statement).getValues().get(0).size();
                break;
            case SELECT:
                parameterCount = ((SelectWrapper) statement).getWhereValues().size();
                break;
            case UPDATE:
                parameterCount = ((UpdateWrapper) statement).getValues().size()
                        + ((UpdateWrapper) statement).getWhereValues().size();
                break;
            default:
                throw new SQLException("Cannot determine the type of the statement.");
        }
        return parameterCount;
    }

    @Override
    public void close() {
        connection = null;
    }

    @Override
    public ResultSet executeQuery() throws SQLException {
        return executeQuery(preparedStatement);
    }

    @Override
    public int executeUpdate() throws SQLException {
        return executeUpdate(preparedStatement);
    }

    @Override
    public ResultSet executeQuery(final String sql) throws SQLException {
        BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        if (!(TypeOfStatement.SELECT.equals(statement.getTypeOfStatement()))) {
            throw new SQLException("Cannot execute executeQuery() because statement was not a Select statement.");
        }
        statement = PreparedStatementCreator.getPreparedSelectStatement((SelectWrapper) statement, this);
        resultSet = statementServiceManager.selectFromTable((SelectWrapper) statement);
        return resultSet;
    }

    @Override
    public int executeUpdate(final String sql) throws SQLException {
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
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
                final DeleteWrapper preparedDeleteStatement = PreparedStatementCreator.getPreparedDeleteStatement(
                        (DeleteWrapper) statement, this);
                updateCount = statementServiceManager.deleteFromTable(preparedDeleteStatement);
                break;
            case DROP_DATABASE:
                updateCount = statementServiceManager.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                updateCount = statementServiceManager.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                final InsertWrapper preparedInsertStatement = PreparedStatementCreator.getPreparedInsertStatement(
                        (InsertWrapper) statement, this);
                updateCount = statementServiceManager.insertIntoTable(preparedInsertStatement);
                break;
            case UPDATE:
                final UpdateWrapper preparedUpdateStatement = PreparedStatementCreator.getPreparedUpdateStatement(
                        (UpdateWrapper) statement, this);
                updateCount = statementServiceManager.updateTable(preparedUpdateStatement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
        return updateCount;
    }

    private void executeQuery(final BaseStatement statement) throws SQLException {
        final SelectWrapper preparedSelectStatement = PreparedStatementCreator.getPreparedSelectStatement(
                (SelectWrapper) statement, this);
        resultSet = statementServiceManager.selectFromTable(preparedSelectStatement);
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
                final DeleteWrapper preparedDeleteStatement = PreparedStatementCreator.getPreparedDeleteStatement(
                        (DeleteWrapper) statement, this);
                statementServiceManager.deleteFromTable(preparedDeleteStatement);
                break;
            case DROP_DATABASE:
                statementServiceManager.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                statementServiceManager.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                final InsertWrapper preparedInsertStatement = PreparedStatementCreator.getPreparedInsertStatement(
                        (InsertWrapper) statement, this);
                statementServiceManager.insertIntoTable(preparedInsertStatement);
                break;
            case UPDATE:
                final UpdateWrapper preparedUpdateStatement = PreparedStatementCreator.getPreparedUpdateStatement(
                        (UpdateWrapper) statement, this);
                statementServiceManager.updateTable(preparedUpdateStatement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
    }

    @Override
    public boolean execute(final String sql) throws SQLException {
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        if (TypeOfStatement.SELECT.equals(statement.getTypeOfStatement())) {
            final SelectWrapper preparedSelectStatement = PreparedStatementCreator.getPreparedSelectStatement(
                    (SelectWrapper) statement, this);
            executeQuery(preparedSelectStatement);
            return true;
        } else {
            executeUpdate(statement);
            return false;
        }
    }

    @Override
    public void setNull(final int parameterIndex, final int sqlType) {
        parameters[parameterIndex] = null;
    }

    @Override
    public void setBoolean(final int parameterIndex, final boolean x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setByte(final int parameterIndex, final byte x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setShort(final int parameterIndex, final short x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setInt(final int parameterIndex, final int x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setLong(final int parameterIndex, final long x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setFloat(final int parameterIndex, final float x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setDouble(final int parameterIndex, final double x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setBigDecimal(final int parameterIndex, final BigDecimal x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setString(final int parameterIndex, final String x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setBytes(final int parameterIndex, final byte[] x) {
        parameters[parameterIndex - 1] = Base64.getEncoder().encodeToString(x);
    }

    @Override
    public void setDate(final int parameterIndex, final Date x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setTime(final int parameterIndex, final Time x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setTimestamp(final int parameterIndex, final Timestamp x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setObject(final int parameterIndex, final Object x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setRef(final int parameterIndex, final Ref x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setClob(final int parameterIndex, final Clob x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setArray(final int parameterIndex, final Array x) {
        parameters[parameterIndex - 1] = x;
    }

    @Override
    public void setURL(final int parameterIndex, final URL x) {
        parameters[parameterIndex - 1] = x;
    }

    private void setInputStream(final int parameterIndex, final InputStream x) throws SQLException {
        if (x == null) {
            parameters[parameterIndex - 1] = null;
            return;
        }
        try {
            final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            int data;
            while ((data = x.read()) != -1) {
                byteArrayOutputStream.write(data);
            }
            final byte[] byteArray = byteArrayOutputStream.toByteArray();
            x.close();
            byteArrayOutputStream.close();
            parameters[parameterIndex - 1] = Base64.getEncoder().encodeToString(byteArray);
        } catch (final IOException e) {
            throw new SQLException("Couldn't set InputStream. \n" + e.getMessage());
        }
    }

    @Override
    public void setBinaryStream(final int parameterIndex, final InputStream x) throws SQLException {
        setInputStream(parameterIndex, x);
    }

    @Override
    public void setBlob(final int parameterIndex, final InputStream x) throws SQLException {
        setInputStream(parameterIndex, x);
    }

    @Override
    public void clearParameters() {
        Arrays.fill(parameters, null);
    }

    @Override
    public ResultSet getResultSet() {
        return resultSet;
    }

    @Override
    public boolean execute() throws SQLException {
        return execute(preparedStatement);
    }

    @Override
    public ResultSetMetaData getMetaData() {
        return new JfsqlResultSetMetaData((JfsqlResultSet) resultSet);
    }

    @Override
    public Connection getConnection() {
        return connection;
    }

    // Unsupported operations

    @Override
    public void setBlob(final int parameterIndex, final Blob x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setAsciiStream(final int parameterIndex, final InputStream x, final int length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setUnicodeStream(final int parameterIndex, final InputStream x, final int length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setBinaryStream(final int parameterIndex, final InputStream x, final int length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setObject(final int parameterIndex, final Object x, final int targetSqlType)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void addBatch() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setCharacterStream(final int parameterIndex, final Reader reader, final int length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setDate(final int parameterIndex, final Date x, final Calendar cal)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setTime(final int parameterIndex, final Time x, final Calendar cal)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setTimestamp(final int parameterIndex, final Timestamp x, final Calendar cal)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNull(final int parameterIndex, final int sqlType, final String typeName)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public ParameterMetaData getParameterMetaData() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setRowId(final int parameterIndex, final RowId x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNString(final int parameterIndex, final String value) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNCharacterStream(final int parameterIndex, final Reader value, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNClob(final int parameterIndex, final NClob value) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setClob(final int parameterIndex, final Reader reader, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setBlob(final int parameterIndex, final InputStream inputStream, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNClob(final int parameterIndex, final Reader reader, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setSQLXML(final int parameterIndex, final SQLXML xmlObject) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setObject(final int parameterIndex, final Object x, final int targetSqlType, final int scaleOrLength)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setAsciiStream(final int parameterIndex, final InputStream x, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setBinaryStream(final int parameterIndex, final InputStream x, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setCharacterStream(final int parameterIndex, final Reader reader, final long length)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setAsciiStream(final int parameterIndex, final InputStream x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setCharacterStream(final int parameterIndex, final Reader reader)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNCharacterStream(final int parameterIndex, final Reader value)
            throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setClob(final int parameterIndex, final Reader reader) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNClob(final int parameterIndex, final Reader reader) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getMaxFieldSize() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setMaxFieldSize(final int max) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getMaxRows() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setMaxRows(final int max) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setEscapeProcessing(final boolean enable) throws SQLFeatureNotSupportedException {
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
    public void cancel() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public SQLWarning getWarnings() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void clearWarnings() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setCursorName(final String name) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean getMoreResults() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getFetchDirection() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setFetchDirection(final int direction) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getFetchSize() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setFetchSize(final int rows) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getResultSetConcurrency() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getResultSetType() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void addBatch(final String sql) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void clearBatch() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int[] executeBatch() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean getMoreResults(final int current) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public ResultSet getGeneratedKeys() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int executeUpdate(final String sql, final int autoGeneratedKeys) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int executeUpdate(final String sql, final int[] columnIndexes) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int executeUpdate(final String sql, final String[] columnNames) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean execute(final String sql, final int autoGeneratedKeys) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean execute(final String sql, final int[] columnIndexes) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean execute(final String sql, final String[] columnNames) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getResultSetHoldability() throws SQLFeatureNotSupportedException {
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
    public void setPoolable(final boolean poolable) throws SQLFeatureNotSupportedException {
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
    public <T> T unwrap(final Class<T> iface) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isWrapperFor(final Class<?> iface) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }
}
