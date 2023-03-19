package com.github.jfsql.driver.core;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.util.PropertiesReader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class JfsqlConnectionTest {

    private static Connection connection;
    private static Statement statement;

    @BeforeAll
    static void beforeAll() throws SQLException {
        connection = new JfsqlConnection(TestUtils.DATABASE_PATH, new PropertiesReader(null));
        statement = connection.createStatement();
    }

    @AfterAll
    static void afterAll() throws SQLException {
        statement.execute("DROP DATABASE [" + TestUtils.DATABASE_PATH + "]");
    }

    @Test
    void testCreateStatement() {
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_UPDATABLE));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.createStatement(ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.createStatement(ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_UPDATABLE));
    }

    @Test
    void testIsValid() throws SQLException {
        assertTrue(connection.isValid(0));
    }

    @Test
    void testNativeSQL() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.nativeSQL(null));
    }

    @Test
    void testIsClosed() throws SQLException {
        assertFalse(connection.isClosed());
    }

    @Test
    void testGetCatalog() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.getCatalog());
    }

    @Test
    void testSetCatalog() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setCatalog(null));
    }

    @Test
    void testGetWarnings() throws SQLException {
        assertNull(connection.getWarnings());
    }

    @Test
    void testClearWarnings() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.clearWarnings());
    }

    @Test
    void testGetTypeMap() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.getTypeMap());
    }

    @Test
    void testSetTypeMap() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setTypeMap(null));
    }

    @Test
    void testGetHoldability() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.getHoldability());
    }

    @Test
    void testSetHoldability() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setHoldability(0));
    }

    @Test
    void testSavepoint() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setSavepoint());
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setSavepoint(null));
    }

    @Test
    void testReleaseSavepoint() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.releaseSavepoint(null));
    }

    @Test
    void testPrepareCall() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.prepareCall(null));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareCall(null, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareCall(null, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareCall(null, ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareCall(null, ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_UPDATABLE));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareCall(null, ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareCall(null, ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_UPDATABLE));
    }

    @Test
    void testPrepareStatement() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.prepareStatement(null, new int[0]));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, Statement.RETURN_GENERATED_KEYS));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, Statement.NO_GENERATED_KEYS));
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.prepareStatement(null, new String[0]));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_UPDATABLE));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY));
        assertThrows(SQLFeatureNotSupportedException.class,
            () -> connection.prepareStatement(null, ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_UPDATABLE));
    }

    @Test
    void testCreateClob() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.createClob());
    }

    @Test
    void testCreateBlob() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.createBlob());
    }

    @Test
    void testCreateNClob() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.createNClob());
    }

    @Test
    void testCreateSQLXML() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.createSQLXML());
    }

    @Test
    void testGetClientInfo() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.getClientInfo());
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.getClientInfo(null));
    }

    @Test
    void testSetClientInfo() {
        assertThrows(UnsupportedOperationException.class, () -> connection.setClientInfo(null));
        assertThrows(UnsupportedOperationException.class, () -> connection.setClientInfo(null, null));
    }

    @Test
    void testCreateArrayOf() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.createArrayOf(null, null));
    }

    @Test
    void testCreateStruct() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.createStruct(null, null));
    }

    @Test
    void testGetSchema() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.getSchema());
    }

    @Test
    void testSetSchema() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setSchema(null));
    }

    @Test
    void testAbort() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.abort(null));
    }

    @Test
    void testSetNetworkTimeout() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.setNetworkTimeout(null, 0));
    }

    @Test
    void testGetNetworkTimeout() throws SQLException {
        assertEquals(0, connection.getNetworkTimeout());
    }

    @Test
    void testUnwrap() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.unwrap(null));
    }

    @Test
    void testIsWrapperFor() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.isWrapperFor(null));
    }

    @Test
    void testRollback() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> connection.rollback(null));
    }
}