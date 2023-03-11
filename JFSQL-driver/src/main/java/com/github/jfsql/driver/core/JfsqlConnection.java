package com.github.jfsql.driver.core;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.factories.CacheFactory;
import com.github.jfsql.driver.factories.ReaderFactory;
import com.github.jfsql.driver.factories.TransactionFactory;
import com.github.jfsql.driver.factories.WriterFactory;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.statements.StatementManager;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.util.PropertiesReader;
import lombok.Getter;
import lombok.Setter;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Executor;

@Getter
@Setter
public class JfsqlConnection implements Connection {

    private final Path url;
    private final Reader reader;
    private final Writer writer;
    private final StatementManager statementManager;
    private final Transaction transaction;
    private final Cache cache;
    private JfsqlStatement statement;
    private JfsqlPreparedStatement preparedStatement;
    private DatabaseMetaData metaData;
    private int transactionIsolation;
    private boolean readOnly;

    public JfsqlConnection(final Path url) throws SQLException {
        cache = CacheFactory.createCache(PropertiesReader.getProperty("statement.caching"));
        reader = ReaderFactory.createReader(PropertiesReader.getProperty("persistence"));
        writer = WriterFactory.createWriter(PropertiesReader.getProperty("persistence"));
        final Database database = getDatabase(url);
        this.url = database.getUrl();
        transaction = TransactionFactory.createTransactionManager(PropertiesReader.getProperty("transactions"), database, reader, writer);
        statementManager = new StatementManager(this, database, reader, writer);
        metaData = new JfsqlDatabaseMetaData(this);
    }

    private Path formDatabaseUrl(final Path url) {
        return Path.of(url + File.separator + url.getFileName() + "." + writer.getFileExtension());
    }

    private Database getDatabase(final Path url) throws SQLException {
        final Database database = new Database(formDatabaseUrl(url));
        if (!database.getUrl().toFile().exists()) {
            initDatabase(database);
        } else {
            openDatabase(database);
        }
        return database;
    }

    private void openDatabase(final Database database) throws SQLException {
        try (final Git ignored = Git.open(database.getUrl().getParent().toFile())) {
            final List<Table> tables = reader.readDatabaseFile(database);
            database.setTables(tables);
        } catch (final IOException e) {
            throw new SQLException("Couldn't open git repository.\n" + e.getMessage());
        }
    }

    public void initDatabase(final Database database) throws SQLException {
        try (final Git git = Git.init().setDirectory(database.getUrl().getParent().toFile()).call()) {
            final List<Table> tables = new ArrayList<>();
            database.setTables(tables);
            writer.writeDatabaseFile(database);
            git.add().addFilepattern(database.getUrl().toFile().getName()).call();
            git.commit().setMessage("Initial commit").call();
        } catch (final GitAPIException e) {
            throw new SQLException("Couldn't init git repository.\n" + e.getMessage());
        }
    }

    @Override
    public Statement createStatement() {
        statement = new JfsqlStatement(this, statementManager, cache);
        return statement;
    }

    @Override
    public PreparedStatement prepareStatement(final String sql) throws SQLException {
        preparedStatement = new JfsqlPreparedStatement(this, sql, statementManager);
        return preparedStatement;
    }

    @Override
    public void close() {
        statement = null;
        preparedStatement = null;
    }

    @Override
    public int getTransactionIsolation() {
        return transactionIsolation;
    }

    @Override
    public void setTransactionIsolation(final int level) {
        transactionIsolation = TRANSACTION_READ_COMMITTED;
    }

    @Override
    public boolean getAutoCommit() {
        return transaction.getAutoCommit();
    }

    @Override
    public void setAutoCommit(final boolean autoCommit) {
        transaction.setAutoCommit(autoCommit);
    }

    @Override
    public void commit() throws SQLException {
        transaction.commit();
    }

    @Override
    public void rollback() throws SQLException {
        transaction.rollback();
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
    public Statement createStatement(final int resultSetType, final int resultSetConcurrency) throws SQLFeatureNotSupportedException {
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
