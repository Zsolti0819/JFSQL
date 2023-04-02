package com.github.jfsql.driver.core;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.factories.CacheFactory;
import com.github.jfsql.driver.factories.DatabaseManagerFactory;
import com.github.jfsql.driver.factories.ReaderFactory;
import com.github.jfsql.driver.factories.TransactionManagerFactory;
import com.github.jfsql.driver.factories.WriterFactory;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.services.StatementServiceManager;
import com.github.jfsql.driver.util.BlobFileNameCreator;
import java.nio.file.Path;
import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.NClob;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Savepoint;
import java.sql.Statement;
import java.sql.Struct;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Executor;
import lombok.Data;

@Data
public class JfsqlConnection implements Connection {

    private final Path url;
    private final TransactionManager transactionManager;
    private final StatementServiceManager statementServiceManager;
    private final BlobFileNameCreator blobFileNameCreator;
    private JfsqlStatement statement;
    private JfsqlPreparedStatement preparedStatement;
    private DatabaseMetaData metaData;

    public JfsqlConnection(final Path url, final PropertiesReader propertiesReader) throws SQLException {
        this.url = url;
        blobFileNameCreator = new BlobFileNameCreator(url, propertiesReader);
        final Cache cache = CacheFactory.createCache(propertiesReader);
        final Reader reader = ReaderFactory.createReader(propertiesReader);
        final Writer writer = WriterFactory.createWriter(propertiesReader);
        final DatabaseManager databaseManager = DatabaseManagerFactory.createDatabaseManager(propertiesReader, url,
            reader, writer);
        transactionManager = TransactionManagerFactory.createTransactionManager(propertiesReader, databaseManager,
            reader, writer);
        statementServiceManager = new StatementServiceManager(databaseManager, cache, transactionManager, reader);
        metaData = new JfsqlDatabaseMetaData(this);
    }

    @Override
    public Statement createStatement() {
        statement = new JfsqlStatement(this, statementServiceManager);
        return statement;
    }

    @Override
    public PreparedStatement prepareStatement(final String sql) throws SQLException {
        preparedStatement = new JfsqlPreparedStatement(this, statementServiceManager, blobFileNameCreator, sql);
        return preparedStatement;
    }

    @Override
    public void close() {
        statement = null;
        preparedStatement = null;
    }

    @Override
    public int getTransactionIsolation() {
        return Connection.TRANSACTION_READ_COMMITTED;
    }

    @Override
    public boolean getAutoCommit() {
        return transactionManager.getAutoCommit();
    }

    @Override
    public void setAutoCommit(final boolean autoCommit) {
        transactionManager.setAutoCommit(autoCommit);
    }

    @Override
    public void commit() throws SQLException {
        transactionManager.commit();
    }

    @Override
    public void rollback() throws SQLException {
        transactionManager.rollback();
    }

    @Override
    public boolean isValid(final int timeout) {
        return true;
    }

    @Override
    public int getNetworkTimeout() {
        return 0;
    }

    @Override
    public boolean isClosed() {
        return false;
    }

    @Override
    public SQLWarning getWarnings() {
        return null;
    }

    // Unsupported operations

    @Override
    public void setReadOnly(final boolean readOnly) throws SQLException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public boolean isReadOnly() {
        return false;
    }

    @Override
    public void setTransactionIsolation(final int level) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public CallableStatement prepareCall(final String sql) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String nativeSQL(final String sql) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getCatalog() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setCatalog(final String catalog) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void clearWarnings() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Statement createStatement(final int resultSetType, final int resultSetConcurrency)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public PreparedStatement prepareStatement(final String sql, final int resultSetType, final int resultSetConcurrency)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public CallableStatement prepareCall(final String sql, final int resultSetType, final int resultSetConcurrency)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Map<String, Class<?>> getTypeMap() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setTypeMap(final Map<String, Class<?>> map) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public int getHoldability() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setHoldability(final int holdability) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Savepoint setSavepoint() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Savepoint setSavepoint(final String name) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void rollback(final Savepoint savepoint) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void releaseSavepoint(final Savepoint savepoint) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Statement createStatement(final int resultSetType, final int resultSetConcurrency,
        final int resultSetHoldability) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public PreparedStatement prepareStatement(final String sql, final int resultSetType, final int resultSetConcurrency,
        final int resultSetHoldability) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public CallableStatement prepareCall(final String sql, final int resultSetType, final int resultSetConcurrency,
        final int resultSetHoldability) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public PreparedStatement prepareStatement(final String sql, final int autoGeneratedKeys)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public PreparedStatement prepareStatement(final String sql, final int[] columnIndexes)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public PreparedStatement prepareStatement(final String sql, final String[] columnNames)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Clob createClob() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Blob createBlob() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public NClob createNClob() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public SQLXML createSQLXML() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setClientInfo(final String name, final String value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getClientInfo(final String name) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Properties getClientInfo() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setClientInfo(final Properties properties) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Array createArrayOf(final String typeName, final Object[] elements) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Struct createStruct(final String typeName, final Object[] attributes)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public String getSchema() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setSchema(final String schema) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void abort(final Executor executor) throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public void setNetworkTimeout(final Executor executor, final int milliseconds)
        throws SQLFeatureNotSupportedException {
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
