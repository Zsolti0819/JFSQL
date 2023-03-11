package com.github.jfsql.driver.core;

import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;

public class JfsqlResultSetMetaData implements ResultSetMetaData {

    private final JfsqlResultSet resultSet;

    JfsqlResultSetMetaData(final JfsqlResultSet resultSet) {
        this.resultSet = resultSet;
    }

    @Override
    public String getColumnLabel(final int column) {
        return getColumnName(column);
    }

    @Override
    public String getColumnName(final int column) {
        return resultSet.getColumnNames()[column - 1];
    }

    @Override
    public String getTableName(final int arg0) {
        return resultSet.getParentTableName();
    }

    @Override
    public int getColumnCount() {
        return resultSet.getColumnNames().length;
    }

    @Override
    public boolean isSearchable(final int arg0) throws SQLException {
        if (arg0 <= 0 || arg0 > getColumnCount()) {
            throw new SQLException();
        }
        return true;
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
    public String getCatalogName(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getColumnClassName(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getColumnDisplaySize(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getColumnType(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getColumnTypeName(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getPrecision(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getScale(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getSchemaName(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isAutoIncrement(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isCaseSensitive(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isCurrency(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isDefinitelyWritable(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int isNullable(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isReadOnly(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isSigned(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isWritable(final int arg0) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

}
