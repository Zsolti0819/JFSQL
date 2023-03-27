package com.github.jfsql.driver.core;

import com.github.jfsql.driver.services.StatementServiceManager;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.Date;
import java.sql.NClob;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.RowId;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Base64;
import java.util.Calendar;
import lombok.Data;

@Data
public class JfsqlPreparedStatement implements PreparedStatement {

    private JfsqlConnection connection;
    private final StatementServiceManager statementServiceManager;
    private final String sql;


    JfsqlPreparedStatement(final JfsqlConnection connection, final StatementServiceManager statementServiceManager,
        final String sql) throws SQLException {
        this.connection = connection;
        this.statementServiceManager = statementServiceManager;
        this.sql = sql;
        statementServiceManager.setParameters(new Object[statementServiceManager.getParameterCount(sql)]);
    }

    @Override
    public void close() {
        connection = null;
    }

    @Override
    public ResultSet executeQuery() throws SQLException {
        return executeQuery(sql);
    }

    @Override
    public int executeUpdate() throws SQLException {
        return executeUpdate(sql);
    }

    @Override
    public ResultSet executeQuery(final String sql) throws SQLException {
        return statementServiceManager.executeQuery(sql, true);
    }

    @Override
    public int executeUpdate(final String sql) throws SQLException {
        return statementServiceManager.executeUpdate(sql, true);
    }

    @Override
    public boolean execute(final String sql) throws SQLException {
        return statementServiceManager.execute(sql, true);
    }

    @Override
    public void setNull(final int parameterIndex, final int sqlType) {
        statementServiceManager.getParameters()[parameterIndex] = null;
    }

    @Override
    public void setBoolean(final int parameterIndex, final boolean x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setByte(final int parameterIndex, final byte x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setShort(final int parameterIndex, final short x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setInt(final int parameterIndex, final int x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setLong(final int parameterIndex, final long x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setFloat(final int parameterIndex, final float x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setDouble(final int parameterIndex, final double x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setBigDecimal(final int parameterIndex, final BigDecimal x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setString(final int parameterIndex, final String x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setBytes(final int parameterIndex, final byte[] x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = Base64.getEncoder().encodeToString(x);
    }

    @Override
    public void setDate(final int parameterIndex, final Date x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setTime(final int parameterIndex, final Time x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setTimestamp(final int parameterIndex, final Timestamp x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setObject(final int parameterIndex, final Object x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setRef(final int parameterIndex, final Ref x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setClob(final int parameterIndex, final Clob x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setArray(final int parameterIndex, final Array x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public void setURL(final int parameterIndex, final URL x) {
        statementServiceManager.getParameters()[parameterIndex - 1] = x;
    }

    @Override
    public int getUpdateCount() {
        return statementServiceManager.getUpdateCount();
    }

    private void setInputStream(final int parameterIndex, final InputStream x) throws SQLException {
        if (x == null) {
            statementServiceManager.getParameters()[parameterIndex - 1] = null;
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
            statementServiceManager.getParameters()[parameterIndex - 1] = Base64.getEncoder().encodeToString(byteArray);
        } catch (final IOException e) {
            throw new SQLException("Couldn't set InputStream.\n" + e.getMessage());
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
        Arrays.fill(statementServiceManager.getParameters(), null);
    }

    @Override
    public ResultSet getResultSet() {
        return statementServiceManager.getResultSet();
    }

    @Override
    public boolean execute() throws SQLException {
        return execute(sql);
    }

    @Override
    public ResultSetMetaData getMetaData() {
        return new JfsqlResultSetMetaData((JfsqlResultSet) statementServiceManager.getResultSet());
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
