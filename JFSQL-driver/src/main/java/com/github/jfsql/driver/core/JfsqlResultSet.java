package com.github.jfsql.driver.core;

import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Date;
import java.sql.NClob;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.RowId;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.Data;

@Data
public class JfsqlResultSet implements ResultSet {

    private Table table;
    private final String tableName;
    private final List<String> columnNames;
    private final List<String> columnTypes;
    private List<Entry> entries;
    private int currentEntry = 0;

    public JfsqlResultSet(final Table table) {
        this.table = table;
        tableName = table.getName();
        entries = table.getEntries();
        columnNames = new ArrayList<>(table.getColumnsAndTypes().keySet());
        columnTypes = new ArrayList<>(table.getColumnsAndTypes().values());
    }

    private int getColumnIndex(final String columnName) throws SQLException {
        for (int i = 0; i < columnNames.size(); i++) {
            if (Objects.equals(columnNames.get(i), columnName)) {
                return i;
            }
        }
        throw new SQLException("Column '" + columnName + "' doesn't exist in the ResultSet.");
    }

    private String getValue(final int row, final int column) {
        final Entry entry = entries.get(row);
        final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
        final List<String> entryValues = new ArrayList<>(columnsAndValues.values());
        return entryValues.get(column);
    }

    private String getBlobValue(final int row, final int column) {
        final Entry entry = entries.get(row);
        final Map<String, LargeObject> columnsAndBlobs = entry.getColumnsAndBlobs();
        final List<LargeObject> entryValues = new ArrayList<>(columnsAndBlobs.values());
        final LargeObject largeObject = entryValues.get(column);
        return largeObject.getValue();
    }

    @Override
    public boolean next() {
        currentEntry++;
        return currentEntry <= entries.size();
    }

    @Override
    public void close() {
        table = null;
    }

    @Override
    public boolean wasNull() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean getBoolean(final int columnIndex) {
        final String value = getValue(currentEntry - 1, columnIndex - 1);
        if (value == null) {
            return false;
        }
        return Boolean.parseBoolean(value);
    }

    @Override
    public boolean getBoolean(final String columnLabel) throws SQLException {
        final int columnIndex = getColumnIndex(columnLabel);
        return getBoolean(columnIndex + 1);
    }

    @Override
    public int getInt(final int columnIndex) {
        final String value = getValue(currentEntry - 1, columnIndex - 1);
        if (value == null) {
            return 0;
        }
        return Integer.parseInt(value);
    }

    @Override
    public int getInt(final String columnLabel) throws SQLException {
        final int columnIndex = getColumnIndex(columnLabel);
        return getInt(columnIndex + 1);
    }

    @Override
    public long getLong(final int columnIndex) {
        final String value = getValue(currentEntry - 1, columnIndex - 1);
        if (value == null) {
            return 0;
        }
        return Long.parseLong(value);
    }

    @Override
    public long getLong(final String columnLabel) throws SQLException {
        final int columnIndex = getColumnIndex(columnLabel);
        return getLong(columnIndex + 1);
    }

    @Override
    public double getDouble(final int columnIndex) {
        final String value = getValue(currentEntry - 1, columnIndex - 1);
        if (value == null) {
            return 0;
        }
        return Double.parseDouble(value);
    }

    @Override
    public double getDouble(final String columnLabel) throws SQLException {
        final int columnIndex = getColumnIndex(columnLabel);
        return getDouble(columnIndex + 1);
    }

    @Override
    public byte[] getBytes(final int columnIndex) {
        final String path = getValue(currentEntry - 1, columnIndex - 1);
        if (path == null) {
            return new byte[0];
        }
        final String value = getBlobValue(currentEntry - 1, columnIndex - 1);
        return Base64.getDecoder().decode(value);
    }

    @Override
    public byte[] getBytes(final String columnLabel) throws SQLException {
        final int columnIndex = getColumnIndex(columnLabel);
        return getBytes(columnIndex + 1);
    }

    @Override
    public String getString(final int columnIndex) {
        return getValue(currentEntry - 1, columnIndex - 1);
    }

    @Override
    public String getString(final String columnLabel) throws SQLException {
        final int columnIndex = getColumnIndex(columnLabel);
        return getString(columnIndex + 1);
    }

    @Override
    public ResultSetMetaData getMetaData() {
        return new JfsqlResultSetMetaData(this);
    }

    @Override
    public int findColumn(final String columnLabel) throws SQLException {
        return (getColumnIndex(columnLabel) + 1);
    }

    @Override
    public boolean isBeforeFirst() {
        if (entries.isEmpty()) {
            return false;
        }
        return currentEntry == 0;
    }

    @Override
    public boolean isAfterLast() {
        if (entries.isEmpty()) {
            return false;
        }
        return currentEntry == entries.size() + 1;
    }

    @Override
    public boolean isFirst() {
        return currentEntry == 1 && !entries.isEmpty();
    }

    @Override
    public boolean isLast() {
        return (currentEntry == entries.size()) && (!entries.isEmpty());
    }

    @Override
    public void beforeFirst() {
        currentEntry = 0;
    }

    @Override
    public void afterLast() {
        currentEntry = entries.size() + 1;
    }

    @Override
    public boolean first() {
        return absolute(1);
    }

    @Override
    public boolean last() {
        return absolute(-1);
    }

    @Override
    public boolean absolute(final int row) {
        currentEntry = row < 0 ? entries.size() + row + 1 : row;
        return currentEntry > 0 && currentEntry <= entries.size();
    }

    @Override
    public boolean previous() {
        currentEntry--;
        return currentEntry > 0;
    }

    @Override
    public Statement getStatement() {
        return null;
    }

    @Override
    public int getFetchDirection() {
        return FETCH_FORWARD;
    }

    // Unsupported operations

    @Override
    public void setFetchDirection(final int direction) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Object getObject(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Object getObject(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public byte getByte(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public byte getByte(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public short getShort(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public short getShort(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public float getFloat(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public float getFloat(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Date getDate(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Date getDate(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Time getTime(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Time getTime(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Timestamp getTimestamp(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Timestamp getTimestamp(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public InputStream getAsciiStream(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public InputStream getAsciiStream(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @SuppressWarnings("squid:CallToDeprecatedMethod")
    @Override
    public InputStream getUnicodeStream(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @SuppressWarnings("squid:CallToDeprecatedMethod")
    @Override
    public InputStream getUnicodeStream(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public InputStream getBinaryStream(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public InputStream getBinaryStream(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @SuppressWarnings("squid:CallToDeprecatedMethod")
    @Override
    public BigDecimal getBigDecimal(final int columnIndex, final int scale) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public BigDecimal getBigDecimal(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public BigDecimal getBigDecimal(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @SuppressWarnings("squid:CallToDeprecatedMethod")
    @Override
    public BigDecimal getBigDecimal(final String columnLabel, final int scale) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Reader getCharacterStream(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Reader getCharacterStream(final String columnLabel) throws SQLFeatureNotSupportedException {
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
    public String getCursorName() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean relative(final int rows) throws SQLFeatureNotSupportedException {
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
    public int getType() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getConcurrency() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean rowUpdated() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean rowInserted() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean rowDeleted() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNull(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBoolean(final int columnIndex, final boolean x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateByte(final int columnIndex, final byte x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateShort(final int columnIndex, final short x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateInt(final int columnIndex, final int x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateLong(final int columnIndex, final long x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateFloat(final int columnIndex, final float x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateDouble(final int columnIndex, final double x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBigDecimal(final int columnIndex, final BigDecimal x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateString(final int columnIndex, final String x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBytes(final int columnIndex, final byte[] x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateDate(final int columnIndex, final Date x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateTime(final int columnIndex, final Time x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateTimestamp(final int columnIndex, final Timestamp x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateAsciiStream(final int columnIndex, final InputStream x, final int length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBinaryStream(final int columnIndex, final InputStream x, final int length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateCharacterStream(final int columnIndex, final Reader x, final int length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateObject(final int columnIndex, final Object x, final int scaleOrLength)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateObject(final int columnIndex, final Object x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNull(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBoolean(final String columnLabel, final boolean x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateByte(final String columnLabel, final byte x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateShort(final String columnLabel, final short x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateInt(final String columnLabel, final int x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateLong(final String columnLabel, final long x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateFloat(final String columnLabel, final float x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateDouble(final String columnLabel, final double x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBigDecimal(final String columnLabel, final BigDecimal x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateString(final String columnLabel, final String x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBytes(final String columnLabel, final byte[] x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateDate(final String columnLabel, final Date x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateTime(final String columnLabel, final Time x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateTimestamp(final String columnLabel, final Timestamp x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateAsciiStream(final String columnLabel, final InputStream x, final int length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBinaryStream(final String columnLabel, final InputStream x, final int length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateCharacterStream(final String columnLabel, final Reader reader, final int length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateObject(final String columnLabel, final Object x, final int scaleOrLength)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateObject(final String columnLabel, final Object x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void insertRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void deleteRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void refreshRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void cancelRowUpdates() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void moveToInsertRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void moveToCurrentRow() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Object getObject(final int columnIndex, final Map<String, Class<?>> map)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Ref getRef(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Blob getBlob(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Clob getClob(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Array getArray(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Object getObject(final String columnLabel, final Map<String, Class<?>> map)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Ref getRef(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Blob getBlob(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Clob getClob(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Array getArray(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Date getDate(final int columnIndex, final Calendar cal) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Date getDate(final String columnLabel, final Calendar cal) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Time getTime(final int columnIndex, final Calendar cal) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Time getTime(final String columnLabel, final Calendar cal) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Timestamp getTimestamp(final int columnIndex, final Calendar cal) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Timestamp getTimestamp(final String columnLabel, final Calendar cal) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public URL getURL(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public URL getURL(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateRef(final int columnIndex, final Ref x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateRef(final String columnLabel, final Ref x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBlob(final int columnIndex, final Blob x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBlob(final String columnLabel, final Blob x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateClob(final int columnIndex, final Clob x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateClob(final String columnLabel, final Clob x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateArray(final int columnIndex, final Array x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateArray(final String columnLabel, final Array x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public RowId getRowId(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public RowId getRowId(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateRowId(final int columnIndex, final RowId x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateRowId(final String columnLabel, final RowId x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getHoldability() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isClosed() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNString(final int columnIndex, final String nString) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNString(final String columnLabel, final String nString) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNClob(final int columnIndex, final NClob nClob) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNClob(final String columnLabel, final NClob nClob) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public NClob getNClob(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public NClob getNClob(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public SQLXML getSQLXML(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public SQLXML getSQLXML(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateSQLXML(final int columnIndex, final SQLXML xmlObject) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateSQLXML(final String columnLabel, final SQLXML xmlObject) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getNString(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getNString(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Reader getNCharacterStream(final int columnIndex) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Reader getNCharacterStream(final String columnLabel) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNCharacterStream(final int columnIndex, final Reader x, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNCharacterStream(final String columnLabel, final Reader reader, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateAsciiStream(final int columnIndex, final InputStream x, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBinaryStream(final int columnIndex, final InputStream x, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateCharacterStream(final int columnIndex, final Reader x, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateAsciiStream(final String columnLabel, final InputStream x, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBinaryStream(final String columnLabel, final InputStream x, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateCharacterStream(final String columnLabel, final Reader reader, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBlob(final int columnIndex, final InputStream inputStream, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBlob(final String columnLabel, final InputStream inputStream, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateClob(final int columnIndex, final Reader reader, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateClob(final String columnLabel, final Reader reader, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNClob(final int columnIndex, final Reader reader, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNClob(final String columnLabel, final Reader reader, final long length)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNCharacterStream(final int columnIndex, final Reader x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNCharacterStream(final String columnLabel, final Reader reader)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateAsciiStream(final int columnIndex, final InputStream x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBinaryStream(final int columnIndex, final InputStream x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateCharacterStream(final int columnIndex, final Reader x) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateAsciiStream(final String columnLabel, final InputStream x)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBinaryStream(final String columnLabel, final InputStream x)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateCharacterStream(final String columnLabel, final Reader reader)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBlob(final int columnIndex, final InputStream inputStream)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateBlob(final String columnLabel, final InputStream inputStream)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateClob(final int columnIndex, final Reader reader) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateClob(final String columnLabel, final Reader reader) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNClob(final int columnIndex, final Reader reader) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void updateNClob(final String columnLabel, final Reader reader) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public <T> T getObject(final int columnIndex, final Class<T> type) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public <T> T getObject(final String columnLabel, final Class<T> type) throws SQLFeatureNotSupportedException {
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
