package com.github.jfsql.driver.core;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.jfsql.driver.services.StatementServiceManager;
import com.github.jfsql.driver.util.BlobFileNameCreator;
import java.io.Reader;
import java.sql.Blob;
import java.sql.NClob;
import java.sql.ResultSet;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import java.sql.Types;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@SuppressWarnings("unused")
@ExtendWith(MockitoExtension.class)
class JfsqlPreparedStatementTest {

    @Mock
    private StatementServiceManager statementServiceManager;

    @Mock
    private BlobFileNameCreator blobFileNameCreator;

    @Mock
    private JfsqlConnection connection;

    @InjectMocks
    private JfsqlPreparedStatement preparedStatement;

    @Test
    void setBlob() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setBlob(1, (Blob) null));
    }

    @Test
    void setAsciiStream() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setAsciiStream(1, null));
    }

    @Test
    void setUnicodeStream() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setUnicodeStream(1, null, 0));
    }

    @Test
    void setBinaryStream() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setBinaryStream(1, null, 0));
    }

    @Test
    void setObject() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setObject(1, null, 0));
    }

    @Test
    void addBatch() {
        assertThrows(SQLFeatureNotSupportedException.class, preparedStatement::addBatch);
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.addBatch(null));
    }

    @Test
    void setCharacterStream() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setCharacterStream(1, null));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> preparedStatement.setCharacterStream(1, Reader.nullReader(), 0));
    }

    @Test
    void setDate() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setDate(1, null, null));
    }

    @Test
    void setTime() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setTime(1, null, null));
    }

    @Test
    void setTimestamp() {
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> preparedStatement.setTimestamp(1, null, null));
    }

    @Test
    void setNull() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setNull(1, Types.NULL, null));
    }

    @Test
    void getParameterMetaData() {
        assertThrows(SQLFeatureNotSupportedException.class, preparedStatement::getParameterMetaData);
    }

    @Test
    void setRowId() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setRowId(1, null));
    }

    @Test
    void setNString() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setNString(1, null));
    }

    @Test
    void setNCharacterStream() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setNCharacterStream(1, null));
    }

    @Test
    void setClob() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setClob(0, Reader.nullReader()));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setClob(0, null, 0));
    }

    @Test
    void setNClob() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setNClob(0, (NClob) null));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setNClob(0, Reader.nullReader()));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setNClob(0, null, 0));
    }

    @Test
    void getMaxFieldSize() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getMaxFieldSize());
    }

    @Test
    void setMaxFieldSize() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setMaxFieldSize(0));
    }

    @Test
    void getMaxRows() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getMaxRows());
    }

    @Test
    void setMaxRows() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setMaxRows(0));
    }

    @Test
    void setEscapeProcessing() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setEscapeProcessing(true));
    }

    @Test
    void getQueryTimeout() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getQueryTimeout());
    }

    @Test
    void setQueryTimeout() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setQueryTimeout(0));
    }

    @Test
    void cancel() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.cancel());
    }

    @Test
    public void getWarnings() {
        assertNull(preparedStatement.getWarnings());
    }

    @Test
    void clearWarnings() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.clearWarnings());
    }

    @Test
    void setCursorName() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setCursorName(null));
    }

    @Test
    void getMoreResults() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getMoreResults());
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getMoreResults(Statement.CLOSE_ALL_RESULTS));
    }

    @Test
    void getFetchDirection() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getFetchDirection());
    }

    @Test
    void setFetchDirection() {
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> preparedStatement.setFetchDirection(ResultSet.FETCH_REVERSE));
    }

    @Test
    void getFetchSize() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getFetchSize());
    }

    @Test
    void setFetchSize() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setFetchSize(0));
    }

    @Test
    void getResultSetConcurrency() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getResultSetConcurrency());
    }

    @Test
    void getResultSetType() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getResultSetType());
    }

    @Test
    void clearBatch() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.clearBatch());
    }

    @Test
    void executeBatch() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.executeBatch());
    }

    @Test
    public void getGeneratedKeys() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getGeneratedKeys());
    }

    @Test
    void executeUpdate() {
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> preparedStatement.executeUpdate(null, Statement.RETURN_GENERATED_KEYS));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.executeUpdate(null, new int[0]));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.executeUpdate(null, new String[0]));
    }

    @Test
    void execute() {
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> preparedStatement.execute(null, Statement.RETURN_GENERATED_KEYS));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.execute(null, new int[0]));
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.execute(null, new String[0]));
    }

    @Test
    void getResultSetHoldability() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.getResultSetHoldability());
    }

    @Test
    void isClosed() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.isClosed());
    }

    @Test
    void isPoolable() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.isPoolable());
    }

    @Test
    void setPoolable() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.setPoolable(true));
    }

    @Test
    void closeOnCompletion() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.closeOnCompletion());
    }

    @Test
    void isCloseOnCompletion() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.isCloseOnCompletion());
    }

    @Test
    public void unwrap() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.unwrap(null));
    }

    @Test
    void isWrapperFor() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> preparedStatement.isWrapperFor(null));
    }

}