package com.github.jfsql.driver.core;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.db.DatabaseManager;
import java.sql.DatabaseMetaData;
import java.sql.SQLFeatureNotSupportedException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@SuppressWarnings("unused")
@ExtendWith(MockitoExtension.class)
class JfsqlDatabaseMetaDataTest {

    @Mock
    private JfsqlConnection connection;
    @Mock
    private DatabaseManager databaseManager;
    @InjectMocks
    private JfsqlDatabaseMetaData metaData;


    @AfterAll
    static void afterAll() {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void allProceduresAreCallable() {
        assertFalse(metaData.allProceduresAreCallable());
    }

    @Test
    void allTablesAreSelectable() {
        assertTrue(metaData.allTablesAreSelectable());
    }

    @Test
    void getUrl() {
        when(metaData.getURL()).thenReturn("someUrl");
        assertEquals("someUrl", metaData.getURL());
    }

    @Test
    void getUserName() {
        assertNull(metaData.getUserName());
    }

    @Test
    void isReadOnly() {
        assertFalse(metaData.isReadOnly());
    }

    @Test
    void nullsAreSortedHigh() {
        assertFalse(metaData.nullsAreSortedHigh());
    }

    @Test
    void nullsAreSortedLow() {
        assertFalse(metaData.nullsAreSortedLow());
    }

    @Test
    void nullsAreSortedAtStart() {
        assertFalse(metaData.nullsAreSortedAtStart());
    }

    @Test
    void nullsAreSortedAtEnd() {
        assertFalse(metaData.nullsAreSortedAtEnd());
    }

    @Test
    void getDatabaseProductName() {
        assertEquals("JFSQL", metaData.getDatabaseProductName());
    }

    @Test
    void getDatabaseProductVersion() {
        assertNull(metaData.getDatabaseProductVersion());
    }

    @Test
    void getDriverName() {
        assertEquals("JFSQL-driver", metaData.getDriverName());
    }

    @Test
    void getDriverVersion() {
        assertEquals("1.0", metaData.getDriverVersion());
    }

    @Test
    void getDriverMajorVersion() {
        assertEquals(1, metaData.getDriverMajorVersion());
    }

    @Test
    void getDriverMinorVersion() {
        assertEquals(0, metaData.getDriverMinorVersion());
    }

    @Test
    void usesLocalFiles() {
        assertTrue(metaData.usesLocalFiles());
    }

    @Test
    void usesLocalFilePerTable() {
        assertTrue(metaData.usesLocalFilePerTable());
    }

    @Test
    void supportsMixedCaseIdentifiers() {
        assertFalse(metaData.supportsMixedCaseIdentifiers());
    }

    @Test
    void storesUpperCaseIdentifiers() {
        assertFalse(metaData.storesUpperCaseIdentifiers());
    }

    @Test
    void storesLowerCaseIdentifiers() {
        assertFalse(metaData.storesLowerCaseIdentifiers());
    }

    @Test
    void storesMixedCaseIdentifiers() {
        assertFalse(metaData.storesMixedCaseIdentifiers());
    }

    @Test
    void supportsMixedCaseQuotedIdentifiers() {
        assertFalse(metaData.supportsMixedCaseQuotedIdentifiers());
    }

    @Test
    void storesUpperCaseQuotedIdentifiers() {
        assertFalse(metaData.storesUpperCaseQuotedIdentifiers());
    }

    @Test
    void storesLowerCaseQuotedIdentifiers() {
        assertFalse(metaData.storesLowerCaseQuotedIdentifiers());
    }

    @Test
    void storesMixedCaseQuotedIdentifiers() {
        assertFalse(metaData.storesMixedCaseQuotedIdentifiers());
    }

    @Test
    void getIdentifierQuoteString() {
        assertEquals("'", metaData.getIdentifierQuoteString());
    }

    @Test
    void getSQLKeywords() {
        assertNull(metaData.getSQLKeywords());
    }

    @Test
    void getNumericFunctions() {
        assertNull(metaData.getNumericFunctions());
    }

    @Test
    void getStringFunctions() {
        assertNull(metaData.getStringFunctions());
    }

    @Test
    void getSystemFunctions() {
        assertNull(metaData.getSystemFunctions());
    }

    @Test
    void getTimeDateFunctions() {
        assertNull(metaData.getTimeDateFunctions());
    }

    @Test
    void getSearchStringEscape() {
        assertNull(metaData.getSearchStringEscape());
    }

    @Test
    void getExtraNameCharacters() {
        assertNull(metaData.getExtraNameCharacters());
    }

    @Test
    void supportsAlterTableWithAddColumn() {
        assertFalse(metaData.supportsAlterTableWithAddColumn());
    }

    @Test
    void supportsAlterTableWithDropColumn() {
        assertFalse(metaData.supportsAlterTableWithDropColumn());
    }

    @Test
    void supportsColumnAliasing() {
        assertFalse(metaData.supportsColumnAliasing());
    }

    @Test
    void nullPlusNonNullIsNull() {
        assertFalse(metaData.nullPlusNonNullIsNull());
    }

    @Test
    void supportsConvert() {
        assertFalse(metaData.supportsConvert());
        assertFalse(metaData.supportsConvert(0, 0));
    }

    @Test
    void supportsTableCorrelationNames() {
        assertFalse(metaData.supportsTableCorrelationNames());
    }

    @Test
    void supportsDifferentTableCorrelationNames() {
        assertFalse(metaData.supportsDifferentTableCorrelationNames());
    }

    @Test
    void supportsExpressionsInOrderBy() {
        assertFalse(metaData.supportsExpressionsInOrderBy());
    }

    @Test
    void supportsOrderByUnrelated() {
        assertFalse(metaData.supportsOrderByUnrelated());
    }

    @Test
    void supportsGroupBy() {
        assertFalse(metaData.supportsGroupBy());
    }

    @Test
    void supportsGroupByUnrelated() {
        assertFalse(metaData.supportsGroupByUnrelated());
    }

    @Test
    void supportsGroupByBeyondSelect() {
        assertFalse(metaData.supportsGroupByBeyondSelect());
    }

    @Test
    void supportsLikeEscapeClause() {
        assertFalse(metaData.supportsLikeEscapeClause());
    }

    @Test
    void supportsMultipleResultSets() {
        assertFalse(metaData.supportsMultipleResultSets());
    }

    @Test
    void supportsMultipleTransactions() {
        assertFalse(metaData.supportsMultipleTransactions());
    }

    @Test
    void supportsNonNullableColumns() {
        assertTrue(metaData.supportsNonNullableColumns());
    }

    @Test
    void supportsMinimumSQLGrammar() {
        assertFalse(metaData.supportsMinimumSQLGrammar());
    }

    @Test
    void supportsCoreSQLGrammar() {
        assertFalse(metaData.supportsCoreSQLGrammar());
    }

    @Test
    void supportsExtendedSQLGrammar() {
        assertFalse(metaData.supportsExtendedSQLGrammar());
    }

    @Test
    void supportsANSI92EntryLevelSQL() {
        assertFalse(metaData.supportsANSI92EntryLevelSQL());
    }

    @Test
    void supportsANSI92IntermediateSQL() {
        assertFalse(metaData.supportsANSI92IntermediateSQL());
    }

    @Test
    void supportsANSI92FullSQL() {
        assertFalse(metaData.supportsANSI92FullSQL());
    }

    @Test
    void supportsIntegrityEnhancementFacility() {
        assertFalse(metaData.supportsIntegrityEnhancementFacility());
    }

    @Test
    void supportsOuterJoins() {
        assertFalse(metaData.supportsOuterJoins());
    }

    @Test
    void supportsFullOuterJoins() {
        assertFalse(metaData.supportsFullOuterJoins());
    }

    @Test
    void supportsLimitedOuterJoins() {
        assertFalse(metaData.supportsLimitedOuterJoins());
    }

    @Test
    void getSchemaTerm() {
        assertNull(metaData.getSchemaTerm());
    }

    @Test
    void getProcedureTerm() {
        assertNull(metaData.getProcedureTerm());
    }

    @Test
    void getCatalogTerm() {
        assertNull(metaData.getCatalogTerm());
    }

    @Test
    void isCatalogAtStart() {
        assertFalse(metaData.isCatalogAtStart());
    }

    @Test
    void getCatalogSeparator() {
        assertNull(metaData.getCatalogSeparator());
    }

    @Test
    void supportsSchemasInDataManipulation() {
        assertFalse(metaData.supportsSchemasInDataManipulation());
    }

    @Test
    void supportsSchemasInProcedureCalls() {
        assertFalse(metaData.supportsSchemasInProcedureCalls());
    }

    @Test
    void supportsSchemasInTableDefinitions() {
        assertFalse(metaData.supportsSchemasInTableDefinitions());
    }

    @Test
    void supportsSchemasInIndexDefinitions() {
        assertFalse(metaData.supportsSchemasInIndexDefinitions());
    }

    @Test
    void supportsSchemasInPrivilegeDefinitions() {
        assertFalse(metaData.supportsSchemasInPrivilegeDefinitions());
    }

    @Test
    void supportsCatalogsInDataManipulation() {
        assertFalse(metaData.supportsCatalogsInDataManipulation());
    }

    @Test
    void supportsCatalogsInProcedureCalls() {
        assertFalse(metaData.supportsCatalogsInProcedureCalls());
    }

    @Test
    void supportsCatalogsInTableDefinitions() {
        assertFalse(metaData.supportsCatalogsInTableDefinitions());
    }

    @Test
    void supportsCatalogsInIndexDefinitions() {
        assertFalse(metaData.supportsCatalogsInIndexDefinitions());
    }

    @Test
    void supportsCatalogsInPrivilegeDefinitions() {
        assertFalse(metaData.supportsCatalogsInPrivilegeDefinitions());
    }

    @Test
    void supportsPositionedDelete() {
        assertFalse(metaData.supportsPositionedDelete());
    }

    @Test
    void supportsPositionedUpdate() {
        assertFalse(metaData.supportsPositionedUpdate());
    }

    @Test
    void supportsSelectForUpdate() {
        assertFalse(metaData.supportsSelectForUpdate());
    }

    @Test
    void supportsStoredProcedures() {
        assertFalse(metaData.supportsStoredProcedures());
    }

    @Test
    void supportsSubqueriesInComparisons() {
        assertFalse(metaData.supportsSubqueriesInComparisons());
    }

    @Test
    void supportsSubqueriesInExists() {
        assertFalse(metaData.supportsSubqueriesInExists());
    }

    @Test
    void supportsSubqueriesInIns() {
        assertFalse(metaData.supportsSubqueriesInIns());
    }

    @Test
    void supportsSubqueriesInQuantifieds() {
        assertFalse(metaData.supportsSubqueriesInQuantifieds());
    }

    @Test
    void supportsCorrelatedSubqueries() {
        assertFalse(metaData.supportsCorrelatedSubqueries());
    }

    @Test
    void supportsUnion() {
        assertFalse(metaData.supportsUnion());
    }

    @Test
    void supportsUnionAll() {
        assertFalse(metaData.supportsUnionAll());
    }

    @Test
    void supportsOpenCursorsAcrossCommit() {
        assertFalse(metaData.supportsOpenCursorsAcrossCommit());
    }

    @Test
    void supportsOpenCursorsAcrossRollback() {
        assertFalse(metaData.supportsOpenCursorsAcrossRollback());
    }

    @Test
    void supportsOpenStatementsAcrossCommit() {
        assertFalse(metaData.supportsOpenStatementsAcrossCommit());
    }

    @Test
    void supportsOpenStatementsAcrossRollback() {
        assertFalse(metaData.supportsOpenStatementsAcrossRollback());
    }

    @Test
    void getMaxBinaryLiteralLength() {
        assertEquals(0, metaData.getMaxBinaryLiteralLength());
    }

    @Test
    void getMaxCharLiteralLength() {
        assertEquals(0, metaData.getMaxCharLiteralLength());
    }

    @Test
    void getMaxColumnNameLength() {
        assertEquals(0, metaData.getMaxColumnNameLength());
    }

    @Test
    void getMaxColumnsInGroupBy() {
        assertEquals(0, metaData.getMaxColumnsInGroupBy());
    }

    @Test
    void getMaxColumnsInIndex() {
        assertEquals(0, metaData.getMaxColumnsInIndex());
    }

    @Test
    void getMaxColumnsInOrderBy() {
        assertEquals(0, metaData.getMaxColumnsInOrderBy());
    }

    @Test
    void getMaxColumnsInSelect() {
        assertEquals(0, metaData.getMaxColumnsInSelect());
    }

    @Test
    void getMaxColumnsInTable() {
        assertEquals(0, metaData.getMaxColumnsInTable());
    }

    @Test
    void getMaxConnections() {
        assertEquals(0, metaData.getMaxConnections());
    }

    @Test
    void getMaxCursorNameLength() {
        assertEquals(0, metaData.getMaxCursorNameLength());
    }

    @Test
    void getMaxIndexLength() {
        assertEquals(0, metaData.getMaxIndexLength());
    }

    @Test
    void getMaxSchemaNameLength() {
        assertEquals(0, metaData.getMaxSchemaNameLength());
    }

    @Test
    void getMaxProcedureNameLength() {
        assertEquals(0, metaData.getMaxProcedureNameLength());
    }

    @Test
    void getMaxCatalogNameLength() {
        assertEquals(0, metaData.getMaxCatalogNameLength());
    }

    @Test
    void getMaxRowSize() {
        assertEquals(0, metaData.getMaxRowSize());
    }

    @Test
    void doesMaxRowSizeIncludeBlobs() {
        assertFalse(metaData.doesMaxRowSizeIncludeBlobs());
    }

    @Test
    void getMaxStatementLength() {
        assertEquals(0, metaData.getMaxStatementLength());
    }

    @Test
    void getMaxStatements() {
        assertEquals(0, metaData.getMaxStatements());
    }

    @Test
    void getMaxTableNameLength() {
        assertEquals(0, metaData.getMaxTableNameLength());
    }

    @Test
    void getMaxTablesInSelect() {
        assertEquals(0, metaData.getMaxTablesInSelect());
    }

    @Test
    void getMaxUserNameLength() {
        assertEquals(0, metaData.getMaxUserNameLength());
    }

    @Test
    void getDefaultTransactionIsolation() {
        assertEquals(0, metaData.getDefaultTransactionIsolation());
    }

    @Test
    void supportsTransactions() {
        assertTrue(metaData.supportsTransactions());
    }

    @Test
    void supportsTransactionIsolationLevel() {
        assertFalse(metaData.supportsTransactionIsolationLevel(0));
    }

    @Test
    void supportsDataDefinitionAndDataManipulationTransactions() {
        assertTrue(metaData.supportsDataDefinitionAndDataManipulationTransactions());
    }

    @Test
    void supportsDataManipulationTransactionsOnly() {
        assertFalse(metaData.supportsDataManipulationTransactionsOnly());
    }

    @Test
    void dataDefinitionCausesTransactionCommit() {
        assertFalse(metaData.dataDefinitionCausesTransactionCommit());
    }

    @Test
    void dataDefinitionIgnoredInTransactions() {
        assertFalse(metaData.dataDefinitionIgnoredInTransactions());
    }

    @Test
    void getProcedures() {
        assertNull(metaData.getProcedures(null, null, null));
    }

    @Test
    void getProcedureColumns() {
        assertNull(metaData.getProcedureColumns(null, null, null, null));
    }

    @Test
    void getTables() {
        assertNull(metaData.getTables(null, null, null, null));
    }

    @Test
    void getSchemas() {
        assertNull(metaData.getSchemas());
        assertNull(metaData.getSchemas(null, null));
    }

    @Test
    void getCatalogs() {
        assertNull(metaData.getCatalogs());
    }

    @Test
    void getTableTypes() {
        assertNull(metaData.getTableTypes());
    }

    @Test
    void getColumns() {
        assertNull(metaData.getColumns(null, null, null, null));
    }

    @Test
    void getColumnPrivileges() {
        assertNull(metaData.getColumnPrivileges(null, null, null, null));
    }

    @Test
    void getTablePrivileges() {
        assertNull(metaData.getTablePrivileges(null, null, null));
    }

    @Test
    void getBestRowIdentifier() {
        assertNull(metaData.getBestRowIdentifier(null, null, null, 0, false));
    }

    @Test
    void getVersionColumns() {
        assertNull(metaData.getVersionColumns(null, null, null));
    }

    @Test
    void getPrimaryKeys() {
        assertNull(metaData.getPrimaryKeys(null, null, null));
    }

    @Test
    void getImportedKeys() {
        assertNull(metaData.getImportedKeys(null, null, null));
    }

    @Test
    void getExportedKeys() {
        assertNull(metaData.getExportedKeys(null, null, null));
    }

    @Test
    void getCrossReference() {
        assertNull(metaData.getCrossReference(null, null, null, null, null, null));
    }

    @Test
    void getTypeInfo() {
        assertNull(metaData.getTypeInfo());
    }

    @Test
    void getIndexInfo() {
        assertNull(metaData.getIndexInfo(null, null, null, false, false));
    }

    @Test
    void supportsResultSetType() {
        assertFalse(metaData.supportsResultSetType(0));
    }

    @Test
    void supportsResultSetConcurrency() {
        assertFalse(metaData.supportsResultSetConcurrency(0, 0));
    }

    @Test
    void ownUpdatesAreVisible() {
        assertFalse(metaData.ownUpdatesAreVisible(0));
    }

    @Test
    void ownDeletesAreVisible() {
        assertFalse(metaData.ownDeletesAreVisible(0));
    }

    @Test
    void ownInsertsAreVisible() {
        assertFalse(metaData.ownInsertsAreVisible(0));
    }

    @Test
    void othersUpdatesAreVisible() {
        assertFalse(metaData.othersUpdatesAreVisible(0));
    }

    @Test
    void othersDeletesAreVisible() {
        assertFalse(metaData.othersDeletesAreVisible(0));
    }

    @Test
    void othersInsertsAreVisible() {
        assertFalse(metaData.othersInsertsAreVisible(0));
    }

    @Test
    void updatesAreDetected() {
        assertFalse(metaData.updatesAreDetected(0));
    }

    @Test
    void deletesAreDetected() {
        assertFalse(metaData.deletesAreDetected(0));
    }

    @Test
    void insertsAreDetected() {
        assertFalse(metaData.insertsAreDetected(0));
    }

    @Test
    void supportsBatchUpdates() {
        assertTrue(metaData.supportsBatchUpdates());
    }

    @Test
    void getUDTs() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getUDTs(null, null, null, new int[0]));
    }

    @Test
    void getConnection() {
        assertNotNull(metaData.getConnection());
    }

    @Test
    void supportsSavepoints() {
        assertFalse(metaData.supportsSavepoints());
    }

    @Test
    void supportsNamedParameters() {
        assertFalse(metaData.supportsNamedParameters());
    }

    @Test
    void supportsMultipleOpenResults() {
        assertFalse(metaData.supportsMultipleOpenResults());
    }

    @Test
    void supportsGetGeneratedKeys() {
        assertFalse(metaData.supportsGetGeneratedKeys());
    }

    @Test
    void getSuperTypes() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getSuperTypes(null, null, null));
    }

    @Test
    void getSuperTables() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getSuperTables(null, null, null));
    }

    @Test
    void getAttributes() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getAttributes(null, null, null, null));
    }

    @Test
    void supportsResultSetHoldability() {
        assertFalse(metaData.supportsResultSetHoldability(0));
    }

    @Test
    void getResultSetHoldability() {
        assertEquals(0, metaData.getResultSetHoldability());
    }

    @Test
    void getDatabaseMajorVersion() {
        assertEquals(1, metaData.getDatabaseMajorVersion());
    }

    @Test
    void getDatabaseMinorVersion() {
        assertEquals(0, metaData.getDatabaseMinorVersion());
    }

    @Test
    void getJDBCMajorVersion() {
        assertEquals(1, metaData.getJDBCMajorVersion());
    }

    @Test
    void getJDBCMinorVersion() {
        assertEquals(0, metaData.getJDBCMinorVersion());
    }

    @Test
    void getSQLStateType() {
        assertEquals(DatabaseMetaData.sqlStateSQL99, metaData.getSQLStateType());
    }

    @Test
    void locatorsUpdateCopy() {
        assertFalse(metaData.locatorsUpdateCopy());
    }

    @Test
    void supportsStatementPooling() {
        assertFalse(metaData.supportsStatementPooling());
    }

    @Test
    void getRowIdLifetime() {
        assertNull(metaData.getRowIdLifetime());
    }

    @Test
    void supportsStoredFunctionsUsingCallSyntax() {
        assertFalse(metaData.supportsStoredFunctionsUsingCallSyntax());
    }

    @Test
    void autoCommitFailureClosesAllResultSets() {
        assertFalse(metaData.autoCommitFailureClosesAllResultSets());
    }

    @Test
    void getClientInfoProperties() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getClientInfoProperties());
    }

    @Test
    void getFunctions() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getFunctions(null, null, null));
    }

    @Test
    void getFunctionColumns() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getFunctionColumns(null, null, null, null));
    }

    @Test
    void getPseudoColumns() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.getPseudoColumns(null, null, null, null));
    }

    @Test
    void generatedKeyAlwaysReturned() {
        assertFalse(metaData.generatedKeyAlwaysReturned());
    }

    @Test
    void unwrap() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.unwrap(null));
    }

    @Test
    void isWrapperFor() {
        assertThrows(SQLFeatureNotSupportedException.class, () -> metaData.isWrapperFor(null));
    }
}
