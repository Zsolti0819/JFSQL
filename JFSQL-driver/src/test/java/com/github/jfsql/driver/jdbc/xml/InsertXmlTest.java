package com.github.jfsql.driver.jdbc.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.core.JfsqlResultSet;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class InsertXmlTest {

    private JfsqlConnection connection;
    private Statement statement;

    @BeforeEach
    void setUp() throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", "xml");
        connection = (JfsqlConnection) DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        statement = connection.createStatement();
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
    }

    @AfterEach
    void deleteDatabaseFolder() throws IOException {
        TestUtils.deleteDatabaseDirectory();
    }

    @Test
    void testInsert_simple() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(1, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_preparedStatement_simple() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, null);
        assertEquals(1, preparedStatement.executeUpdate());
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_preparedStatement_blob() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER, file BLOB)");
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "INSERT INTO myTable (id, name, age, file) VALUES (?, ?, ?, ?)");
        preparedStatement.setInt(1, 1);
        preparedStatement.setString(2, "Zsolti");
        preparedStatement.setInt(3, 25);
        preparedStatement.setBinaryStream(4, new FileInputStream(TestUtils.META_INF_DRIVER_FILE_PATH.toFile()));
        assertEquals(1, preparedStatement.executeUpdate());

        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "        <file>" + TestUtils.ENCODED_BLOB_PATH_XML + "</file>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent),
            StringUtils.deleteWhitespace(realFileContent));

        final PreparedStatement selectPreparedStatement = connection.prepareStatement(
            "SELECT file FROM myTable WHERE id = 1");
        final ResultSet resultSet = selectPreparedStatement.executeQuery();
        while (resultSet.next()) {
            final byte[] bytes = resultSet.getBytes("file");
            final FileOutputStream fileOutputStream = new FileOutputStream(TestUtils.BLOB_COPY_FILE_PATH.toFile());
            fileOutputStream.write(bytes);
            fileOutputStream.close();
        }

        assertTrue(TestUtils.BLOB_COPY_FILE_PATH.toFile().exists());
    }

    @Test
    void testInsert_multiRow() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>2</id>\n" +
            "        <name>Tomi</name>\n" +
            "        <age>24</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>3</id>\n" +
            "        <name>Ivan</name>\n" +
            "        <age>26</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_noExplicitColumns() throws SQLException, IOException {
        assumeTrue(connection.getReader() instanceof ReaderXmlImpl);
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)"));
        final String realFileContent = FileUtils.readFileToString(TestUtils.TABLE_XML_FILE_PATH.toFile(),
            StandardCharsets.UTF_8);
        final String expectedFileContent = "" +
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" +
            "<myTable>\n" +
            "    <Entry>\n" +
            "        <id>1</id>\n" +
            "        <name>Zsolti</name>\n" +
            "        <age>25</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>2</id>\n" +
            "        <name>Tomi</name>\n" +
            "        <age>24</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>3</id>\n" +
            "        <name>Ivan</name>\n" +
            "        <age>26</age>\n" +
            "    </Entry>\n" +
            "    <Entry>\n" +
            "        <id>4</id>\n" +
            "        <name>Lukas</name>\n" +
            "        <age>34</age>\n" +
            "    </Entry>\n" +
            "</myTable>\n";
        assertEquals(StringUtils.deleteWhitespace(expectedFileContent), StringUtils.deleteWhitespace(realFileContent));
    }

    @Test
    void testInsert_notValidValue() {
        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES ('a', 'Zsolti', 25)"));
        assertEquals("Some value's type didn't match the type of the column, to which it was intended to be inserted.",
            thrown.getMessage());
    }

    @Test
    void testInsert_columnNotExists() {
        final SQLException thrown = assertThrows(SQLException.class, () -> statement.executeUpdate(
            "INSERT INTO myTable (lol, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals("Some columns entered doesn't exist in \"myTable\".", thrown.getMessage());

    }

    @Test
    void testInsert_increasesResultSetSize() throws SQLException {
        final JfsqlResultSet resultSetBefore = (JfsqlResultSet) statement.executeQuery("SELECT * FROM myTable");
        assertEquals(0, resultSetBefore.getEntries().size());
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        final JfsqlResultSet resultSetAfter = (JfsqlResultSet) statement.executeQuery("SELECT * FROM myTable");
        assertEquals(4, resultSetAfter.getEntries().size());
    }

    @Test
    void testInsert_multiple() throws SQLException {
        assertEquals(2,
            statement.executeUpdate("INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24)"));
        assertEquals(3, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24)"));
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
        assertEquals(2, statement.executeUpdate("INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24)"));
        assertEquals(3,
            statement.executeUpdate("INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24)"));
        assertEquals(4, statement.executeUpdate(
            "INSERT INTO myTable VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 24), (4, 'Lukas', 34)"));
    }

    @Test
    void testInsertPerformance_1000EntriesMultiRow() throws SQLException {
        assertEquals(1000, statement.executeUpdate("INSERT INTO myTable VALUES " +
            "(0, 'jIuBEsnljH', 56), " +
            "(1, 'ikuPOLHSkg', 47), " +
            "(2, 'rTpDbtypIB', 24), " +
            "(3, 'wNlTayaEAB', 9), " +
            "(4, 'ezySWjByDq', 10), " +
            "(5, 'CbyIIrJOiF', 25), " +
            "(6, 'XjckHxVGYH', 68), " +
            "(7, 'HJSKRgwChh', 65), " +
            "(8, 'jHOoopEeOD', 36), " +
            "(9, 'TCwRqkQsjW', 59), " +
            "(10, 'PWHfGeGEXF', 21), " +
            "(11, 'xWcVPCvcpx', 12), " +
            "(12, 'qCWzAgSoXu', 55), " +
            "(13, 'CSRmHGWSsy', 29), " +
            "(14, 'UsEOkSCxpp', 54), " +
            "(15, 'LhnqIBCODN', 52), " +
            "(16, 'BXtXnToYFI', 16), " +
            "(17, 'QYytwNgXlm', 43), " +
            "(18, 'MHMaygbAUl', 9), " +
            "(19, 'mxiVbKdKxs', 55), " +
            "(20, 'OGtBnKFpli', 26), " +
            "(21, 'kpRsWqKPEq', 63), " +
            "(22, 'SfEkmbElnb', 49), " +
            "(23, 'KfcoyJJtkk', 61), " +
            "(24, 'wKPEzujCpv', 38), " +
            "(25, 'UrqPLmoFNl', 2), " +
            "(26, 'xntHeWwhvM', 59), " +
            "(27, 'chjecpAkcX', 12), " +
            "(28, 'yVLPlBDsuZ', 15), " +
            "(29, 'BKFoVKrgBk', 59), " +
            "(30, 'IbeGHXoJbv', 10), " +
            "(31, 'yQBDLEhpcq', 68), " +
            "(32, 'TaVTIgzYDn', 16), " +
            "(33, 'vzDmyIcFWu', 36), " +
            "(34, 'kxnqmLbZxr', 18), " +
            "(35, 'eHAEWLxqPB', 27), " +
            "(36, 'xdhwGoZDPp', 4), " +
            "(37, 'QTtGOKyikT', 54), " +
            "(38, 'cDzOuGKmip', 24), " +
            "(39, 'Hdjqdvbhkh', 33), " +
            "(40, 'lROtgWLHDp', 54), " +
            "(41, 'kdxYfzidxG', 16), " +
            "(42, 'ACHhbONLRx', 30), " +
            "(43, 'gmvbZBpMGo', 63), " +
            "(44, 'aXaceygQkk', 44), " +
            "(45, 'NDPgacLenN', 31), " +
            "(46, 'FwAWozAYMC', 3), " +
            "(47, 'MtkcSfJwFZ', 68), " +
            "(48, 'IdPxRZkkcY', 56), " +
            "(49, 'ulaQHTJzNd', 16), " +
            "(50, 'nEatHczTFv', 35), " +
            "(51, 'EJxoURIjqD', 27), " +
            "(52, 'HVlZhjOYXF', 68), " +
            "(53, 'BGSwcICnmp', 2), " +
            "(54, 'ccVxurlOTo', 47), " +
            "(55, 'iNJAOXnkkS', 40), " +
            "(56, 'bOSJarpIUn', 28), " +
            "(57, 'daZOLOsGry', 47), " +
            "(58, 'OIElBCfrGo', 28), " +
            "(59, 'OXTokGdICn', 19), " +
            "(60, 'peoRXzgGuT', 63), " +
            "(61, 'PqNswECLTi', 21), " +
            "(62, 'PwzbweLJWg', 19), " +
            "(63, 'biagimnLRn', 62), " +
            "(64, 'ExqLdHAScH', 67), " +
            "(65, 'rSNbQWxsti', 66), " +
            "(66, 'SLObSxkSrJ', 26), " +
            "(67, 'YeiNOGzEiz', 11), " +
            "(68, 'rcznZSBBQu', 60), " +
            "(69, 'xSANTxDsRu', 31), " +
            "(70, 'JKsJzSKZKA', 12), " +
            "(71, 'rytKKtwcTm', 64), " +
            "(72, 'ssmjjptGbX', 60), " +
            "(73, 'daomqwKajz', 15), " +
            "(74, 'CGeNRLVODU', 43), " +
            "(75, 'mGXpTyOxsP', 50), " +
            "(76, 'HVUoJFidpQ', 24), " +
            "(77, 'knhuSTgvmn', 17), " +
            "(78, 'JYHjnOdsav', 68), " +
            "(79, 'MdJmXCZEER', 27), " +
            "(80, 'gKPiWagQaQ', 28), " +
            "(81, 'SQzToJJKTx', 3), " +
            "(82, 'MYqDifAkDK', 7), " +
            "(83, 'jDZrtleZvk', 64), " +
            "(84, 'FMyulQbvPA', 39), " +
            "(85, 'ORjPynZlXG', 37), " +
            "(86, 'AnbFWPjyTE', 50), " +
            "(87, 'gTDjSBCEqg', 3), " +
            "(88, 'zUTKnFzHcU', 14), " +
            "(89, 'WNFJexRvaI', 61), " +
            "(90, 'TlEAHcNSdS', 41), " +
            "(91, 'MkFuuVjHeC', 41), " +
            "(92, 'pxfTzxUiAi', 49), " +
            "(93, 'iAtPUSuXfg', 28), " +
            "(94, 'pajBBsiqmB', 51), " +
            "(95, 'zKCqmSqWnA', 41), " +
            "(96, 'lMiwzWWWtA', 25), " +
            "(97, 'ArpiDtbkJS', 22), " +
            "(98, 'icSKuVzAOI', 28), " +
            "(99, 'trlbwfhdge', 8), " +
            "(100, 'LdkqMGBdVa', 56), " +
            "(101, 'thiNrllEYN', 3), " +
            "(102, 'khSRZYQnvh', 12), " +
            "(103, 'mbpSSynRJE', 41), " +
            "(104, 'anaeWuUYXX', 68), " +
            "(105, 'ycwezwShPe', 14), " +
            "(106, 'MzXUedPNwe', 63), " +
            "(107, 'NoRAUFiivZ', 41), " +
            "(108, 'BRrDGiPeQV', 43), " +
            "(109, 'bLLlJAFHAz', 3), " +
            "(110, 'bKWooIQKMU', 46), " +
            "(111, 'ACdozCaDUz', 52), " +
            "(112, 'WLhRiuihPt', 19), " +
            "(113, 'MkLSUmuOZg', 65), " +
            "(114, 'FgkACPcNxP', 29), " +
            "(115, 'xGyOAjMIdO', 32), " +
            "(116, 'ZjfqsIXGoc', 57), " +
            "(117, 'pjCPQfxDpm', 55), " +
            "(118, 'MzMFAcTkqW', 65), " +
            "(119, 'PYkDdkCyNN', 43), " +
            "(120, 'uytleDrIxr', 41), " +
            "(121, 'xlGGcfyWBS', 52), " +
            "(122, 'gPlHzCLskK', 23), " +
            "(123, 'xblEsYgNPw', 62), " +
            "(124, 'PXYCGQgppq', 54), " +
            "(125, 'XmfWIUJnGt', 57), " +
            "(126, 'xBKKWYdpOr', 13), " +
            "(127, 'KlIQXEsxHB', 50), " +
            "(128, 'resWJnJFoO', 13), " +
            "(129, 'JtaXttzMCG', 26), " +
            "(130, 'nBgfrGjLUm', 19), " +
            "(131, 'vcrUVdbAPD', 54), " +
            "(132, 'ZHrKSYjQvi', 37), " +
            "(133, 'pEtlYnJQgO', 39), " +
            "(134, 'XcsqRmVHvb', 16), " +
            "(135, 'LnsZuwWeSC', 15), " +
            "(136, 'fQrXCBiTMa', 29), " +
            "(137, 'vGuHVuzqOb', 55), " +
            "(138, 'JjIKZpEWcI', 4), " +
            "(139, 'dJPFDXOaLF', 12), " +
            "(140, 'bwfiNXuKTq', 43), " +
            "(141, 'iFMGeUzqGb', 49), " +
            "(142, 'PZUGMGPQdq', 53), " +
            "(143, 'jzUmqdBfHI', 43), " +
            "(144, 'iYzaWKBZgO', 32), " +
            "(145, 'ODuDeBYToB', 67), " +
            "(146, 'nNEAJAqLbh', 59), " +
            "(147, 'hUTsTXXTjE', 37), " +
            "(148, 'xibmIWMVWK', 54), " +
            "(149, 'lufwZTqLcr', 41), " +
            "(150, 'JUQAhNpBno', 49), " +
            "(151, 'hGLnMKQjgc', 41), " +
            "(152, 'tfjcvtFKzz', 39), " +
            "(153, 'TwMOdDeFKz', 62), " +
            "(154, 'DihgqbObls', 57), " +
            "(155, 'ncAPvHEDdd', 64), " +
            "(156, 'IDPTEHloGH', 12), " +
            "(157, 'APzROjxdLD', 50), " +
            "(158, 'sTZfhgkDAy', 21), " +
            "(159, 'lXYYOnddlD', 18), " +
            "(160, 'MPCnVdzkdR', 4), " +
            "(161, 'PHitYtYTom', 4), " +
            "(162, 'wVKLxTRQan', 52), " +
            "(163, 'NxgkAeCBKs', 39), " +
            "(164, 'CMaTvwhYpw', 4), " +
            "(165, 'LNIrnuCYic', 22), " +
            "(166, 'ozgPiuserg', 28), " +
            "(167, 'NCdqFkQGYK', 53), " +
            "(168, 'BvpdXarKEW', 45), " +
            "(169, 'yStnUqSzYJ', 28), " +
            "(170, 'TaCipJrMdD', 53), " +
            "(171, 'XvveChgNQf', 30), " +
            "(172, 'ZETDxmmwCT', 55), " +
            "(173, 'LLohQYNSpo', 30), " +
            "(174, 'mmADvotjYM', 51), " +
            "(175, 'UjRnhYTFrN', 68), " +
            "(176, 'bVLfZtmFON', 27), " +
            "(177, 'yUsWjvaKmy', 3), " +
            "(178, 'QIGuCsuQxc', 3), " +
            "(179, 'mrggfwBiZm', 13), " +
            "(180, 'QJWGCgDSIo', 14), " +
            "(181, 'WEjtIkiILA', 18), " +
            "(182, 'cyaUtCbWjB', 40), " +
            "(183, 'UuSmGxvAch', 56), " +
            "(184, 'saANHMxdhn', 43), " +
            "(185, 'navAprypID', 58), " +
            "(186, 'aPGeRxktIz', 53), " +
            "(187, 'fMOCnBRHfl', 22), " +
            "(188, 'doHUmCRQac', 67), " +
            "(189, 'UKlMELtYpv', 58), " +
            "(190, 'WtwwYkqrhm', 19), " +
            "(191, 'iRiKeaKbgB', 50), " +
            "(192, 'SLBlTTtZuY', 66), " +
            "(193, 'NMymweoycN', 11), " +
            "(194, 'puijOvaBCs', 38), " +
            "(195, 'zdNOdDKXtc', 40), " +
            "(196, 'qnvkyWCyan', 60), " +
            "(197, 'TkFGRKEdLc', 23), " +
            "(198, 'eQEsEgvMvU', 8), " +
            "(199, 'exIrdYggij', 9), " +
            "(200, 'lRezHYNJoW', 40), " +
            "(201, 'JQTxnebsIP', 56), " +
            "(202, 'VFqtguWkYr', 43), " +
            "(203, 'PpBugtUDme', 30), " +
            "(204, 'doUlIatAzj', 35), " +
            "(205, 'NcOeHsbBwH', 38), " +
            "(206, 'AePybrXfBk', 1), " +
            "(207, 'LcWSgLrFwh', 63), " +
            "(208, 'RPGBPZNDXg', 41), " +
            "(209, 'efosWcgyGR', 49), " +
            "(210, 'lOHnJLLEke', 52), " +
            "(211, 'ySporrqXrw', 39), " +
            "(212, 'SPOOMYOmJQ', 33), " +
            "(213, 'GrZkvgLyRK', 36), " +
            "(214, 'KdZIgDOUSR', 24), " +
            "(215, 'SOtDxcaKsF', 20), " +
            "(216, 'IrnwUNobWt', 14), " +
            "(217, 'vaoTPeUpYA', 30), " +
            "(218, 'ZDQcUrLfzq', 48), " +
            "(219, 'NFvFcQVQVD', 17), " +
            "(220, 'fROPuqEUFf', 33), " +
            "(221, 'MbrWxuWgTT', 61), " +
            "(222, 'meGXKJpdNR', 41), " +
            "(223, 'ySzKQZEQDj', 31), " +
            "(224, 'ERmtftVNtk', 67), " +
            "(225, 'naonnOUAVP', 34), " +
            "(226, 'LuALoGIEBq', 57), " +
            "(227, 'BXOKsAEDjU', 43), " +
            "(228, 'loQAAdeJQP', 31), " +
            "(229, 'ewBRfSTCJl', 47), " +
            "(230, 'qCGehJyVji', 55), " +
            "(231, 'jQzWHCBcwZ', 44), " +
            "(232, 'lEnfvCmIis', 38), " +
            "(233, 'qBLMqvEtDK', 55), " +
            "(234, 'EiZVcyDIFm', 57), " +
            "(235, 'KhxrKSELVk', 29), " +
            "(236, 'UoJKkyGfcJ', 64), " +
            "(237, 'fFZRTSMyXR', 3), " +
            "(238, 'QWozTvEomw', 36), " +
            "(239, 'MmEqDXbwUj', 51), " +
            "(240, 'bNnuOypgII', 67), " +
            "(241, 'teixCImcgm', 59), " +
            "(242, 'uUuSDugybb', 25), " +
            "(243, 'ueWqOjLFcG', 45), " +
            "(244, 'PFICJBxzeX', 15), " +
            "(245, 'LspFwQrJAP', 58), " +
            "(246, 'ESJoivzlFo', 42), " +
            "(247, 'jDAZrHYJhY', 9), " +
            "(248, 'VTyuuhKagf', 29), " +
            "(249, 'nLKRFGBYiL', 32), " +
            "(250, 'ipiKkvkjUF', 32), " +
            "(251, 'KjRwvaehCe', 65), " +
            "(252, 'gQXIgDypyZ', 35), " +
            "(253, 'LurtRVNHdj', 24), " +
            "(254, 'BMmjadTUFm', 44), " +
            "(255, 'kKbzmFROxb', 45), " +
            "(256, 'RFukoCQYoD', 34), " +
            "(257, 'nKHcnsBexV', 31), " +
            "(258, 'GjsVSbTHpf', 16), " +
            "(259, 'cLUrsxPOxt', 44), " +
            "(260, 'MUqtaMXRMB', 40), " +
            "(261, 'UmQsIgYcsb', 33), " +
            "(262, 'HWTuslFbMW', 55), " +
            "(263, 'JEIabKRXHu', 36), " +
            "(264, 'IbywkFgnoL', 11), " +
            "(265, 'clzZVyleIH', 47), " +
            "(266, 'lMJSeWjtWX', 39), " +
            "(267, 'supuqCpfJa', 31), " +
            "(268, 'cLzjNyDAue', 1), " +
            "(269, 'QhUjIMluYR', 16), " +
            "(270, 'uDlDrtLnHr', 45), " +
            "(271, 'edzcdUlWpv', 57), " +
            "(272, 'JmzgELSIGc', 7), " +
            "(273, 'sRsfpJsTea', 6), " +
            "(274, 'NEoKUXIgDZ', 55), " +
            "(275, 'qeWrtVBKXX', 22), " +
            "(276, 'VCygQTfHjf', 32), " +
            "(277, 'LvcfiYvIbo', 51), " +
            "(278, 'uAdinhdZKW', 8), " +
            "(279, 'MlKFhJXLLt', 26), " +
            "(280, 'oiyUCnguyh', 40), " +
            "(281, 'sqDyvZADUi', 10), " +
            "(282, 'FizDvCFdSL', 7), " +
            "(283, 'qBGXNvauNh', 25), " +
            "(284, 'eZtMEhPBvo', 51), " +
            "(285, 'ScUTyZTXLQ', 39), " +
            "(286, 'jbbzNmyUoW', 7), " +
            "(287, 'joCtMQhHox', 4), " +
            "(288, 'yKmxjzddtu', 46), " +
            "(289, 'skvsnLDhHe', 49), " +
            "(290, 'CDOiDVxfDC', 47), " +
            "(291, 'TuhGltkpPq', 1), " +
            "(292, 'RPJDiGUZhV', 54), " +
            "(293, 'cldWUzJwaf', 65), " +
            "(294, 'RMuQzOvIIB', 49), " +
            "(295, 'DGSYzbZrhT', 59), " +
            "(296, 'LlXzBrtxgR', 53), " +
            "(297, 'rNQOomPgKy', 42), " +
            "(298, 'VnitfJbvxt', 41), " +
            "(299, 'RelpzvleLJ', 11), " +
            "(300, 'WZNtHlRDRJ', 34), " +
            "(301, 'ngIdHURZpL', 35), " +
            "(302, 'pIAdIhYCPJ', 10), " +
            "(303, 'TheERSSDto', 17), " +
            "(304, 'pdtAhXYJNy', 39), " +
            "(305, 'ijvVoOQdJl', 50), " +
            "(306, 'WQzJrpTHyu', 40), " +
            "(307, 'NUiJZqepZl', 13), " +
            "(308, 'MQVAUlSYnn', 67), " +
            "(309, 'pEfOeaKGPH', 65), " +
            "(310, 'HoaqKJURyc', 21), " +
            "(311, 'pSlhQpsHtP', 52), " +
            "(312, 'AZIjjyIwgb', 42), " +
            "(313, 'BENxXWhVRw', 63), " +
            "(314, 'YBqgNnliZX', 63), " +
            "(315, 'isKWzuvPET', 1), " +
            "(316, 'hknvJFDegM', 60), " +
            "(317, 'mDydXJkaeA', 58), " +
            "(318, 'SuHcwHfzKk', 3), " +
            "(319, 'kCnvxNnynZ', 16), " +
            "(320, 'plGoLwhJCo', 15), " +
            "(321, 'cexREqZnEX', 53), " +
            "(322, 'aYadnAdVWf', 64), " +
            "(323, 'LSSJjVWeta', 31), " +
            "(324, 'wXkuWvVtgz', 25), " +
            "(325, 'ZgUmDVOhUU', 30), " +
            "(326, 'UcSdgicCes', 68), " +
            "(327, 'yYUSsXLtHe', 36), " +
            "(328, 'FNuxsJaGat', 45), " +
            "(329, 'UCQortUvgv', 41), " +
            "(330, 'BWVomKLgXM', 31), " +
            "(331, 'unpGrmsZuI', 65), " +
            "(332, 'apWuRrWeZn', 32), " +
            "(333, 'JYVSMCKERm', 45), " +
            "(334, 'QmdeIHUKHh', 57), " +
            "(335, 'OLBIrizBGr', 16), " +
            "(336, 'RrSdpASLZg', 53), " +
            "(337, 'qIHumLRQKr', 21), " +
            "(338, 'nWoTDPUXFy', 36), " +
            "(339, 'IcVexlwBCm', 48), " +
            "(340, 'drBefaNiIo', 16), " +
            "(341, 'xConhmGusH', 4), " +
            "(342, 'ZdGcYEyrzb', 21), " +
            "(343, 'wLIYksCemo', 8), " +
            "(344, 'XZZukdjLJa', 40), " +
            "(345, 'YrOzNmxOKi', 34), " +
            "(346, 'dRyWCToyym', 30), " +
            "(347, 'SlKPrHjqOU', 33), " +
            "(348, 'VOrqSrCsqe', 14), " +
            "(349, 'qcLdghZThv', 51), " +
            "(350, 'PBhHcBPvCF', 7), " +
            "(351, 'KYluImzXcd', 39), " +
            "(352, 'ZVswFjSGhz', 60), " +
            "(353, 'THiOyySMTD', 45), " +
            "(354, 'RVLMJmlVst', 52), " +
            "(355, 'hUhNPiVHOZ', 23), " +
            "(356, 'bkuJcpdDJI', 47), " +
            "(357, 'EArqDInuqs', 33), " +
            "(358, 'awfpvjTSFR', 40), " +
            "(359, 'eAPHuQeIBl', 36), " +
            "(360, 'bprxarZXia', 24), " +
            "(361, 'ujhzysBBsx', 54), " +
            "(362, 'qBmnHjxHeK', 52), " +
            "(363, 'MpsNKcBgkh', 38), " +
            "(364, 'SChQoDteZf', 66), " +
            "(365, 'BNcROpLeKR', 35), " +
            "(366, 'HjVamRIIYK', 33), " +
            "(367, 'ydNrbzXyoq', 2), " +
            "(368, 'cnyfIPbyLx', 35), " +
            "(369, 'LgxEzKeiPP', 1), " +
            "(370, 'LtbmcxlGrV', 58), " +
            "(371, 'JepKbzCvLf', 44), " +
            "(372, 'VgLbiGusds', 42), " +
            "(373, 'MbEwsKDutC', 56), " +
            "(374, 'HwjniLnlve', 16), " +
            "(375, 'fFWadnjuZS', 11), " +
            "(376, 'nVVDiWnbJh', 26), " +
            "(377, 'XEUWnvcnmM', 15), " +
            "(378, 'PTNygeyptg', 57), " +
            "(379, 'mzKEWUVAPZ', 14), " +
            "(380, 'wFaksEBHTn', 4), " +
            "(381, 'lSjZHcmKCG', 1), " +
            "(382, 'ohFlhGFTue', 36), " +
            "(383, 'MpoqhLeQjV', 41), " +
            "(384, 'gXszZlgvRi', 4), " +
            "(385, 'ZMXLVNFmTL', 36), " +
            "(386, 'ynwiVkAxax', 47), " +
            "(387, 'IasmBywxFZ', 53), " +
            "(388, 'ExTNpIoHlz', 29), " +
            "(389, 'tePRFKgsak', 37), " +
            "(390, 'AKmarNsHtY', 27), " +
            "(391, 'IhLOrlAbHx', 20), " +
            "(392, 'trdiCTugla', 4), " +
            "(393, 'zUjBbiJQRG', 13), " +
            "(394, 'fVyzJfZpGO', 49), " +
            "(395, 'TUNUxJQtaC', 44), " +
            "(396, 'fKDjuHIBmt', 24), " +
            "(397, 'WTHwnEbOci', 59), " +
            "(398, 'cADiZwFXRB', 54), " +
            "(399, 'XnmrBwpQEu', 9), " +
            "(400, 'tmknkONvhV', 13), " +
            "(401, 'JayGEimtBp', 68), " +
            "(402, 'odhaCZXWgl', 40), " +
            "(403, 'ABXsrBSyet', 62), " +
            "(404, 'bnIuJyHbmC', 60), " +
            "(405, 'FrbgqCPQWe', 49), " +
            "(406, 'vJVqrvTtzh', 38), " +
            "(407, 'fcibNUSCPS', 32), " +
            "(408, 'MqzGfpBjzK', 34), " +
            "(409, 'blBipnNKYX', 41), " +
            "(410, 'FJYRAasFHe', 3), " +
            "(411, 'drLXVoLmSZ', 66), " +
            "(412, 'LPeYWsDJkS', 53), " +
            "(413, 'FoCdWlMLIL', 31), " +
            "(414, 'qQwzDNXorm', 6), " +
            "(415, 'YlJrCgGnla', 10), " +
            "(416, 'urrqIwPtWb', 44), " +
            "(417, 'zfpbNWqVZK', 31), " +
            "(418, 'RCqOkPDXba', 4), " +
            "(419, 'nstbIcpvck', 53), " +
            "(420, 'isgFTzIwHK', 49), " +
            "(421, 'xmEyaUAbVx', 19), " +
            "(422, 'swfqFJDmAu', 51), " +
            "(423, 'wzPdALaBpi', 46), " +
            "(424, 'qvnQxRKyuH', 8), " +
            "(425, 'OJxskbJgml', 50), " +
            "(426, 'gvGrVqaTKK', 61), " +
            "(427, 'UNqvzQTBlO', 35), " +
            "(428, 'fryCzCxZdZ', 48), " +
            "(429, 'ipeURmNlkc', 62), " +
            "(430, 'CsJVxEcVCq', 49), " +
            "(431, 'TJPJmZKqKQ', 31), " +
            "(432, 'DKUZnFFzQa', 2), " +
            "(433, 'WngpWGimVV', 48), " +
            "(434, 'tLDTKPYvuE', 11), " +
            "(435, 'iGqSvuHtJg', 10), " +
            "(436, 'BnfniZtCwM', 61), " +
            "(437, 'TMQYMMxzCF', 31), " +
            "(438, 'hzUEZVJkPH', 55), " +
            "(439, 'rUgFNUpIEs', 38), " +
            "(440, 'DxOjFWvLYl', 39), " +
            "(441, 'wFVYPcRwLe', 48), " +
            "(442, 'NSXanfKcvi', 20), " +
            "(443, 'wQcdkpfulj', 21), " +
            "(444, 'sBcQWgtUzV', 58), " +
            "(445, 'mfSirBupPa', 9), " +
            "(446, 'tGRxHRbdPb', 20), " +
            "(447, 'XzEvCCQNKx', 67), " +
            "(448, 'XObPEdfFsK', 34), " +
            "(449, 'mtXjEWXFJO', 24), " +
            "(450, 'awUYreEcuy', 44), " +
            "(451, 'njhrfylVmy', 50), " +
            "(452, 'DRHjNIGzXK', 26), " +
            "(453, 'FJhVmsuWmy', 35), " +
            "(454, 'lukKLlJyxb', 39), " +
            "(455, 'CfKdmafoBJ', 42), " +
            "(456, 'habAWOXJCt', 7), " +
            "(457, 'CzLPZgGMCQ', 54), " +
            "(458, 'LVNUujDIHj', 42), " +
            "(459, 'AGwXzMupEx', 36), " +
            "(460, 'xCPvcOqojk', 29), " +
            "(461, 'uVWIaHPiCn', 2), " +
            "(462, 'IfuDkTvOgf', 37), " +
            "(463, 'yHvyzVidzE', 24), " +
            "(464, 'aYzUBlVLeg', 56), " +
            "(465, 'WXAjXWpHVI', 14), " +
            "(466, 'JDNmbfWfxu', 22), " +
            "(467, 'ujdKgnhrXP', 5), " +
            "(468, 'EfeGTwAvJo', 56), " +
            "(469, 'XfXGENGXpT', 48), " +
            "(470, 'SzDSMUKGhZ', 5), " +
            "(471, 'UHZWwsgyKk', 7), " +
            "(472, 'yFRywlfrwq', 61), " +
            "(473, 'IpaAwNrCNe', 55), " +
            "(474, 'xovBVenPMI', 37), " +
            "(475, 'EyIdOdKHyB', 62), " +
            "(476, 'yjfxSkvHTn', 37), " +
            "(477, 'SvSrSfAwHz', 11), " +
            "(478, 'mYSzUteyhr', 36), " +
            "(479, 'DgOGPPzYHh', 9), " +
            "(480, 'gPJUjZeiSo', 57), " +
            "(481, 'PUNHVhnSeF', 61), " +
            "(482, 'RhucpdPaYD', 34), " +
            "(483, 'BuzhLYNOlr', 58), " +
            "(484, 'NJjeuMckEd', 47), " +
            "(485, 'zXoMWSbaVg', 21), " +
            "(486, 'hDaPAbvbOZ', 3), " +
            "(487, 'GkHTbesCpK', 32), " +
            "(488, 'nIcIOZzaQl', 7), " +
            "(489, 'IAKxHRMeTa', 25), " +
            "(490, 'oZusmAlzDn', 41), " +
            "(491, 'RJxMlxLQGJ', 57), " +
            "(492, 'KrRbPnsaIQ', 31), " +
            "(493, 'QvVrATgOLL', 59), " +
            "(494, 'RQDDGWFgtA', 45), " +
            "(495, 'VJdMtaKtjO', 63), " +
            "(496, 'TFdkLsPjXz', 18), " +
            "(497, 'PEVkPolgmB', 17), " +
            "(498, 'FtBXifCFgG', 55), " +
            "(499, 'iBfRfeExCV', 62), " +
            "(500, 'OZLSREncTz', 55), " +
            "(501, 'uUrpnDgqYc', 32), " +
            "(502, 'EKcsCjbEde', 45), " +
            "(503, 'kNSZtRlXkI', 47), " +
            "(504, 'VBlrqmsIMG', 40), " +
            "(505, 'vgziKVzQAz', 58), " +
            "(506, 'qFGbFcSFsA', 53), " +
            "(507, 'PbHtOJuylJ', 44), " +
            "(508, 'atAQvjGwlY', 32), " +
            "(509, 'DaXYsKDBwb', 36), " +
            "(510, 'KeiZXoWWkH', 3), " +
            "(511, 'QqLFvYemSC', 17), " +
            "(512, 'aQIBxcVshX', 46), " +
            "(513, 'vHMngnygTQ', 22), " +
            "(514, 'yLyQadjofx', 36), " +
            "(515, 'ZcgRmBNwir', 26), " +
            "(516, 'sOBYyNqVeX', 12), " +
            "(517, 'txDUSGcbgJ', 33), " +
            "(518, 'tovrEPJflJ', 3), " +
            "(519, 'OmQUmXfmAc', 51), " +
            "(520, 'odlEgqBFPJ', 2), " +
            "(521, 'TQmFOjqRvA', 1), " +
            "(522, 'GhbIvcRcHd', 32), " +
            "(523, 'WumBtYNzsg', 24), " +
            "(524, 'MCZeyHMDcp', 62), " +
            "(525, 'SBhiVJsYiA', 65), " +
            "(526, 'HNdtgAPnEI', 67), " +
            "(527, 'sMxEAhJNmc', 23), " +
            "(528, 'PgJsYucrdX', 7), " +
            "(529, 'iPlZwHqmKC', 26), " +
            "(530, 'MhmXMfCllN', 40), " +
            "(531, 'grnGEqXdnl', 55), " +
            "(532, 'qMytgQMnhy', 66), " +
            "(533, 'eHvvkreRKk', 8), " +
            "(534, 'ANWxGTUNAU', 31), " +
            "(535, 'qAwBnzGPgL', 38), " +
            "(536, 'rbimpmWIMW', 48), " +
            "(537, 'tEwwuukLiF', 50), " +
            "(538, 'KZmVexLpEt', 43), " +
            "(539, 'hSLIaeEQjM', 9), " +
            "(540, 'EoSzLICoAd', 8), " +
            "(541, 'ZjDLyCKrWA', 4), " +
            "(542, 'LdcQbcUear', 10), " +
            "(543, 'wRUCOQvsIh', 47), " +
            "(544, 'VLgZejnBTp', 40), " +
            "(545, 'uNDhtmuDuR', 22), " +
            "(546, 'MWQxLLMLff', 40), " +
            "(547, 'FaAxIOoUhW', 15), " +
            "(548, 'laUxhVYezx', 58), " +
            "(549, 'HdFbjYqGtW', 48), " +
            "(550, 'femasfCilD', 35), " +
            "(551, 'zroxXsWvPw', 41), " +
            "(552, 'fwSnDqzNnu', 11), " +
            "(553, 'HQixbanhON', 67), " +
            "(554, 'HLEckGgGDQ', 61), " +
            "(555, 'IonQvXXnJG', 5), " +
            "(556, 'FtgcYzkIBy', 55), " +
            "(557, 'xQaofIPksK', 15), " +
            "(558, 'uuUsJjttrm', 29), " +
            "(559, 'vFRHLmnBhH', 37), " +
            "(560, 'LFDkAcoXuv', 28), " +
            "(561, 'xclXNunmSN', 65), " +
            "(562, 'wdltvjWsaL', 58), " +
            "(563, 'ycOUqyvJLu', 32), " +
            "(564, 'LFXCoaZuoB', 29), " +
            "(565, 'haHoTDLKBw', 62), " +
            "(566, 'ZPGGhVeChj', 39), " +
            "(567, 'CGsfyyjnbf', 3), " +
            "(568, 'ZWnXscmaTQ', 14), " +
            "(569, 'PTPDDQslSO', 54), " +
            "(570, 'UVLpsdxpHv', 68), " +
            "(571, 'HALEBQsfGO', 65), " +
            "(572, 'VZkzohTetp', 53), " +
            "(573, 'jQpjnhzcWL', 68), " +
            "(574, 'fzmlMKtYQA', 5), " +
            "(575, 'zEgUOLWLiR', 20), " +
            "(576, 'wyGeqEGJbP', 62), " +
            "(577, 'MWYhtnXqPl', 12), " +
            "(578, 'fsYMibEyks', 20), " +
            "(579, 'GHHQsJPanW', 6), " +
            "(580, 'WbcJUjvAux', 2), " +
            "(581, 'MVPoijHCns', 10), " +
            "(582, 'McAQhoKrwn', 43), " +
            "(583, 'GWBGwoBeIM', 38), " +
            "(584, 'ZOUIMDcksm', 27), " +
            "(585, 'aTtgZbijHO', 50), " +
            "(586, 'yXKRhiPqVP', 39), " +
            "(587, 'znuNaepBqV', 8), " +
            "(588, 'ouvvBlGcPa', 22), " +
            "(589, 'vCmsHOcJts', 12), " +
            "(590, 'ZWSQoqyPKE', 51), " +
            "(591, 'nhGXhQDhBz', 50), " +
            "(592, 'ljoqrjZFRn', 59), " +
            "(593, 'QngaXQbQQw', 37), " +
            "(594, 'jLrfumbzBr', 3), " +
            "(595, 'tMehbxlEIX', 41), " +
            "(596, 'PoCLTTSTgl', 21), " +
            "(597, 'ufJOaBlmEv', 42), " +
            "(598, 'BLkpJnnihV', 35), " +
            "(599, 'TTHymOtRqZ', 50), " +
            "(600, 'rcxXctoAmc', 37), " +
            "(601, 'mMzBRbPGXM', 53), " +
            "(602, 'rXIvtgQzWj', 64), " +
            "(603, 'AvvDOjZUOO', 1), " +
            "(604, 'qqfvgDWaic', 23), " +
            "(605, 'oNQFpqCqMj', 34), " +
            "(606, 'KaEBKCLBLR', 9), " +
            "(607, 'oeKBojSIgW', 13), " +
            "(608, 'MbZwCgwvNs', 11), " +
            "(609, 'rwgKWIdoWm', 57), " +
            "(610, 'AAyjIeMQQa', 1), " +
            "(611, 'xxcdqqCwWw', 34), " +
            "(612, 'dGSLTtqoGl', 15), " +
            "(613, 'ZJulSOBJin', 50), " +
            "(614, 'WnzntjGbWD', 48), " +
            "(615, 'FmbRqxXjbY', 2), " +
            "(616, 'jXOLOMmlDN', 23), " +
            "(617, 'OOWdOoTSNH', 50), " +
            "(618, 'BcaSBTvYVP', 41), " +
            "(619, 'mHGiLWVWoo', 55), " +
            "(620, 'iuDjbJvnIo', 8), " +
            "(621, 'yUwQfVezTZ', 68), " +
            "(622, 'WjgnAmnwZg', 30), " +
            "(623, 'QtDmhOZlSj', 54), " +
            "(624, 'tXdbaRSBzt', 55), " +
            "(625, 'KNaRxZVDcA', 68), " +
            "(626, 'BlNWgvQafo', 26), " +
            "(627, 'rDoPpClMTP', 15), " +
            "(628, 'JiwKsoEwvT', 9), " +
            "(629, 'WBnldcZKhg', 53), " +
            "(630, 'DeZtEYEuFY', 22), " +
            "(631, 'eGSNeWPDjN', 54), " +
            "(632, 'ScHYvVDFpc', 44), " +
            "(633, 'mNnLFmdmAr', 9), " +
            "(634, 'qfFToTyeaF', 61), " +
            "(635, 'yxuSmbVBGP', 48), " +
            "(636, 'ZrqSNGbaoT', 27), " +
            "(637, 'auBuruhYSk', 61), " +
            "(638, 'RHILhEIOyg', 65), " +
            "(639, 'KdFnDhMyTr', 60), " +
            "(640, 'byheDIVrCj', 21), " +
            "(641, 'hRQCRWphUe', 2), " +
            "(642, 'CaOQVvxBEj', 2), " +
            "(643, 'atilAAKzDQ', 27), " +
            "(644, 'zBSEMSQJeu', 62), " +
            "(645, 'yDaQPwoNbc', 8), " +
            "(646, 'WfpgQdHVpE', 37), " +
            "(647, 'UGapAxcaJi', 35), " +
            "(648, 'npzadVHKbM', 21), " +
            "(649, 'hXaswYrVSD', 21), " +
            "(650, 'tUAxNndeed', 57), " +
            "(651, 'siZWpFDrvQ', 10), " +
            "(652, 'PiKoNNbcbp', 67), " +
            "(653, 'ZoooUxycFr', 60), " +
            "(654, 'nlJIpKwpwE', 29), " +
            "(655, 'zLNpjKPdtT', 8), " +
            "(656, 'lrYojVeXPa', 30), " +
            "(657, 'CQkAPZfnEn', 8), " +
            "(658, 'ZdfjSjAeLT', 13), " +
            "(659, 'QBpZQxVFAX', 52), " +
            "(660, 'OvFIiibrTy', 27), " +
            "(661, 'dQFTbvoHsJ', 14), " +
            "(662, 'BHlnbFFnBp', 12), " +
            "(663, 'KizncPZhMd', 30), " +
            "(664, 'nAPOlehWSQ', 21), " +
            "(665, 'gayZhQIUTU', 48), " +
            "(666, 'qhkvyjhzyV', 30), " +
            "(667, 'PyZrxjCVer', 64), " +
            "(668, 'jfMnJgjOnu', 51), " +
            "(669, 'cjBWJHLcTT', 34), " +
            "(670, 'wJhdquCheS', 21), " +
            "(671, 'HHwURgQMfU', 10), " +
            "(672, 'CQTVHLecTN', 26), " +
            "(673, 'VItBsBRifS', 52), " +
            "(674, 'BNEQbflkvf', 25), " +
            "(675, 'zJZYQJhotx', 52), " +
            "(676, 'HVjWnakbVt', 13), " +
            "(677, 'QjhUPdnbyG', 6), " +
            "(678, 'FBUdBltTBm', 20), " +
            "(679, 'zJXWAUQPkc', 49), " +
            "(680, 'azJFVEBGOb', 31), " +
            "(681, 'yVGYmOFwqe', 50), " +
            "(682, 'OcKjrxcifB', 55), " +
            "(683, 'NDOdfRSVkX', 30), " +
            "(684, 'zzsqgQgFYd', 41), " +
            "(685, 'CRmStCSMDA', 11), " +
            "(686, 'wDvUhMKijH', 32), " +
            "(687, 'yEHkxCDmLf', 15), " +
            "(688, 'AYbvHdKfbH', 15), " +
            "(689, 'MJBfHdatfF', 49), " +
            "(690, 'KFIDCcxrij', 38), " +
            "(691, 'BPkcrhKzma', 43), " +
            "(692, 'xMHQBtdGKS', 33), " +
            "(693, 'aROeVuckGv', 53), " +
            "(694, 'lguhTcDztK', 26), " +
            "(695, 'JhARJiVwNc', 36), " +
            "(696, 'GAwSbbUZez', 19), " +
            "(697, 'qHPlZLjJhu', 2), " +
            "(698, 'WJSsdtHkLh', 47), " +
            "(699, 'GeAqhpVLZu', 28), " +
            "(700, 'yGSKDnYEYN', 7), " +
            "(701, 'KcmXkchwrm', 33), " +
            "(702, 'LZYtIKqaZh', 10), " +
            "(703, 'dOqrZWECFu', 60), " +
            "(704, 'GbQayWqUdz', 25), " +
            "(705, 'jXuZsHyoNw', 33), " +
            "(706, 'AOZQVvvmer', 11), " +
            "(707, 'zlisaZTnNZ', 42), " +
            "(708, 'FTOlBlZJDM', 48), " +
            "(709, 'SEcoTNLfUE', 32), " +
            "(710, 'aSBxdTUAqB', 68), " +
            "(711, 'QuJGHMajvG', 16), " +
            "(712, 'KXjPckjAvT', 11), " +
            "(713, 'OcpyWvrSQG', 39), " +
            "(714, 'QuEHmlTZXL', 27), " +
            "(715, 'mNQhGMUHLI', 29), " +
            "(716, 'FoBEwHlqtD', 17), " +
            "(717, 'OGeaXmWGuN', 46), " +
            "(718, 'OGlMGpYmrm', 12), " +
            "(719, 'MVEirpoOPM', 53), " +
            "(720, 'RsfQOGKxUs', 7), " +
            "(721, 'SbhydtdyZF', 26), " +
            "(722, 'sfJVYkdWkZ', 38), " +
            "(723, 'sUyfQBlByu', 54), " +
            "(724, 'ekHUGlnZIv', 66), " +
            "(725, 'dDbTyqCGCm', 6), " +
            "(726, 'PEsefjORXx', 1), " +
            "(727, 'wcaGCbWeIO', 63), " +
            "(728, 'hDlaCtujAg', 66), " +
            "(729, 'eoqksgbYOP', 15), " +
            "(730, 'czQqyKhMiv', 55), " +
            "(731, 'VecWYJGBuW', 3), " +
            "(732, 'IbqWriAYun', 23), " +
            "(733, 'JXLFakXqBx', 62), " +
            "(734, 'NEoudvpSKd', 65), " +
            "(735, 'dIXDXfuSAy', 27), " +
            "(736, 'ZFOLAlsBpH', 2), " +
            "(737, 'qRVCiVvEmP', 3), " +
            "(738, 'sjJHqDsbuT', 9), " +
            "(739, 'VVAGIRwCSD', 32), " +
            "(740, 'yqOTrpMaPe', 28), " +
            "(741, 'OhhJtffLjU', 43), " +
            "(742, 'xhSpEmeNwS', 30), " +
            "(743, 'oQRQPdWUrS', 60), " +
            "(744, 'uFBFwIztum', 29), " +
            "(745, 'llLBfqLjgn', 58), " +
            "(746, 'bAMsaHmUuL', 17), " +
            "(747, 'jImfFiMMQb', 39), " +
            "(748, 'rcJYnzaKOB', 41), " +
            "(749, 'GlJbgeVlAw', 7), " +
            "(750, 'wrukrJJwPs', 38), " +
            "(751, 'WyargsUPdf', 14), " +
            "(752, 'bDdqWwePrV', 51), " +
            "(753, 'asZrCPxvsg', 47), " +
            "(754, 'vjfwBtkHol', 63), " +
            "(755, 'eMwsMJGMPj', 48), " +
            "(756, 'hjzmIaFWVN', 22), " +
            "(757, 'IPFxwhZtrp', 3), " +
            "(758, 'KkrPackvZb', 33), " +
            "(759, 'yQnnddISYA', 31), " +
            "(760, 'gEMisUSlkL', 61), " +
            "(761, 'UuurHADJsE', 17), " +
            "(762, 'CkcDKuQpZR', 28), " +
            "(763, 'OzxlxuGQxc', 15), " +
            "(764, 'orxRVQHqpy', 12), " +
            "(765, 'bsqIOgDFaz', 32), " +
            "(766, 'iGaPRRMTCz', 37), " +
            "(767, 'PdXCcbSXFz', 6), " +
            "(768, 'tCcwCZRerx', 14), " +
            "(769, 'RaBNhTnHNN', 48), " +
            "(770, 'THlexYBsmV', 10), " +
            "(771, 'WXBKvJtVjL', 59), " +
            "(772, 'dLlCTsOlIv', 21), " +
            "(773, 'tBihLTCdKl', 8), " +
            "(774, 'ioLJFDlCgn', 27), " +
            "(775, 'aWZghIoxFP', 11), " +
            "(776, 'BHTaLDRnRl', 65), " +
            "(777, 'AUaiWXZDkZ', 36), " +
            "(778, 'DDpJqlxhuF', 9), " +
            "(779, 'veKzlAyagB', 10), " +
            "(780, 'fvPdjgyiYF', 61), " +
            "(781, 'plJRncactP', 37), " +
            "(782, 'IZDbWXPxSI', 30), " +
            "(783, 'iFgXPxGEtm', 48), " +
            "(784, 'nYHiwsuMTO', 5), " +
            "(785, 'TOPOsPVNvg', 23), " +
            "(786, 'yxLjkdexDK', 44), " +
            "(787, 'JoUjwyKjiN', 6), " +
            "(788, 'yExVFAGXSu', 30), " +
            "(789, 'VvrejFUtWQ', 59), " +
            "(790, 'QGOahNayZB', 11), " +
            "(791, 'KYekMkubeT', 22), " +
            "(792, 'oAuxMUeFcZ', 44), " +
            "(793, 'izvPYZibAB', 50), " +
            "(794, 'dVteLOdldp', 60), " +
            "(795, 'CWbSQtlVFL', 17), " +
            "(796, 'xiRdDZoRea', 43), " +
            "(797, 'GpHJmvHtwk', 21), " +
            "(798, 'JDUBqNSvFr', 9), " +
            "(799, 'YseCsazWIV', 4), " +
            "(800, 'vstqzIBKnF', 47), " +
            "(801, 'dSAsmKUBKt', 61), " +
            "(802, 'eyAsZnHtsW', 35), " +
            "(803, 'JaLolQUXWD', 57), " +
            "(804, 'tKvAdBqKxS', 50), " +
            "(805, 'xpGioDSWyD', 39), " +
            "(806, 'ZbhpMuFiwh', 47), " +
            "(807, 'DqmXJZrlDr', 34), " +
            "(808, 'AoSsGFyKZU', 55), " +
            "(809, 'pIIcUVTVFe', 18), " +
            "(810, 'hSRrSUgHqf', 25), " +
            "(811, 'AtpLvJvFiN', 3), " +
            "(812, 'JKIQmbNPuA', 62), " +
            "(813, 'QXMEEHCTGr', 19), " +
            "(814, 'ZbRVQUPVwX', 3), " +
            "(815, 'GMTHhzpauN', 60), " +
            "(816, 'FRicPXEIkI', 60), " +
            "(817, 'ZRIDkquQmT', 47), " +
            "(818, 'LvQXGzIaZi', 8), " +
            "(819, 'kyxTZTrFuh', 31), " +
            "(820, 'dhALzRuINN', 5), " +
            "(821, 'mtfMNjxlBz', 27), " +
            "(822, 'VEXDjFHTRc', 27), " +
            "(823, 'ZVlxnJYJel', 9), " +
            "(824, 'UlPTixIZUS', 50), " +
            "(825, 'qcgzTLXUfA', 8), " +
            "(826, 'dLimGZSqEK', 41), " +
            "(827, 'AtuyiwUqrz', 32), " +
            "(828, 'JdgVDPTozX', 39), " +
            "(829, 'URcusigQUO', 4), " +
            "(830, 'WvBpDEEWIZ', 37), " +
            "(831, 'aTDQkIZbKR', 19), " +
            "(832, 'YiZQdOTbXX', 37), " +
            "(833, 'UPAPeMeCXL', 65), " +
            "(834, 'JzDAelXCnF', 58), " +
            "(835, 'ybnEWdfToa', 38), " +
            "(836, 'QuTKoWOUsC', 49), " +
            "(837, 'iFuRwifhCx', 12), " +
            "(838, 'GBGJdtjeXm', 41), " +
            "(839, 'XKbScnAHwX', 5), " +
            "(840, 'zXIfYPFyaW', 7), " +
            "(841, 'ZZNmMLRLIj', 48), " +
            "(842, 'LFvIhvZNvU', 65), " +
            "(843, 'qVWRGjGQfi', 2), " +
            "(844, 'dyqKyOFgmh', 34), " +
            "(845, 'rpEevSQriy', 67), " +
            "(846, 'DJfcqxicxr', 29), " +
            "(847, 'jIZOcxNmzi', 62), " +
            "(848, 'ajxXdFZaXL', 33), " +
            "(849, 'bnxrcwUnGe', 43), " +
            "(850, 'rICkXaPJGr', 42), " +
            "(851, 'WAmMDKUwdL', 68), " +
            "(852, 'GFuKZsODGZ', 65), " +
            "(853, 'dvrIwTfDjW', 21), " +
            "(854, 'UrpUGgxFZP', 23), " +
            "(855, 'NvvPHdrwaD', 3), " +
            "(856, 'mYBTKcGQlP', 63), " +
            "(857, 'zQLIMDCtye', 21), " +
            "(858, 'XZvMFAAVdu', 24), " +
            "(859, 'WDzESjprOQ', 41), " +
            "(860, 'yeFNdREeGO', 44), " +
            "(861, 'IasZvwSgFq', 37), " +
            "(862, 'hmTndgPjiV', 22), " +
            "(863, 'WYZxtBQpfm', 21), " +
            "(864, 'FgfecumTlv', 41), " +
            "(865, 'immXQXOjvm', 68), " +
            "(866, 'BfzltVIilV', 56), " +
            "(867, 'kvmyJyRJTQ', 7), " +
            "(868, 'JKsROhrMzG', 47), " +
            "(869, 'UJaiWHxreQ', 8), " +
            "(870, 'FrBuxxULSr', 4), " +
            "(871, 'cFJrDxeGoT', 41), " +
            "(872, 'QEULQSSrWU', 30), " +
            "(873, 'uIfcgKHTGn', 9), " +
            "(874, 'TMJLjkjHBJ', 64), " +
            "(875, 'WkfZeBZRMa', 8), " +
            "(876, 'khwClKaEcC', 52), " +
            "(877, 'GftLxPbWIG', 22), " +
            "(878, 'QVgFtKxaSi', 51), " +
            "(879, 'UnScFjnJvz', 5), " +
            "(880, 'HVlUCCfQpa', 66), " +
            "(881, 'oHCmFztBLc', 27), " +
            "(882, 'RTiITPrjmk', 34), " +
            "(883, 'uJMwyMhZkj', 66), " +
            "(884, 'BaOcDPHlMI', 66), " +
            "(885, 'BdonzenYie', 10), " +
            "(886, 'nUgScaXjZN', 65), " +
            "(887, 'RdhXjktQdr', 68), " +
            "(888, 'WbcXFAZTXC', 28), " +
            "(889, 'qpiinzbpcM', 46), " +
            "(890, 'zTOsCgrhJY', 50), " +
            "(891, 'GWYpnElKxs', 20), " +
            "(892, 'cGHrJQNXxU', 4), " +
            "(893, 'dwVwFViPcK', 11), " +
            "(894, 'cBQztjFodN', 24), " +
            "(895, 'xRfjilDBfc', 63), " +
            "(896, 'yQKrkVgWhL', 34), " +
            "(897, 'bNLtBQhUPy', 63), " +
            "(898, 'FIAdnQUWYq', 20), " +
            "(899, 'WRoyvyFDKA', 17), " +
            "(900, 'ZPllcBczdH', 3), " +
            "(901, 'dzHkauokbt', 49), " +
            "(902, 'fODuPmoDxI', 52), " +
            "(903, 'eONaOyxYsH', 23), " +
            "(904, 'VhgugoqzGm', 29), " +
            "(905, 'nfILZHqHmX', 4), " +
            "(906, 'UuyUepCpyZ', 25), " +
            "(907, 'OOfPCQSZML', 56), " +
            "(908, 'fAKtqfnWOS', 2), " +
            "(909, 'MxRovsckfu', 54), " +
            "(910, 'mLVzSyWdpu', 46), " +
            "(911, 'XUzEuZTtfE', 60), " +
            "(912, 'cGZctZtKoc', 28), " +
            "(913, 'lMPvkPzsjY', 63), " +
            "(914, 'hxdBeDfcmC', 34), " +
            "(915, 'UwmDCYWfZX', 6), " +
            "(916, 'pLawnFhPXn', 28), " +
            "(917, 'SFgueEZdSL', 23), " +
            "(918, 'ssDzuwKkGv', 33), " +
            "(919, 'cFYiHzeHyV', 66), " +
            "(920, 'ZZIvogkXOe', 68), " +
            "(921, 'YaMpodwPET', 22), " +
            "(922, 'qECctUztBp', 3), " +
            "(923, 'pLovtwIaBs', 23), " +
            "(924, 'UBmOgnmDAq', 66), " +
            "(925, 'TyrAyNOSOs', 37), " +
            "(926, 'rWIvodALSG', 62), " +
            "(927, 'dqqDZDnLCQ', 16), " +
            "(928, 'aQrrqEiyHx', 1), " +
            "(929, 'TJLsIzadRd', 64), " +
            "(930, 'EsdMyYlNIC', 35), " +
            "(931, 'yJQdsQfyfG', 32), " +
            "(932, 'iIFuVMlxUN', 40), " +
            "(933, 'YHCfjVDdLQ', 54), " +
            "(934, 'sKYNcycsMA', 39), " +
            "(935, 'mZpnCgZeRK', 56), " +
            "(936, 'VvujpDnEtA', 68), " +
            "(937, 'knWsgdveAR', 21), " +
            "(938, 'hONyfUPyBz', 66), " +
            "(939, 'AGPKkhEymB', 58), " +
            "(940, 'UnWGVzORBF', 30), " +
            "(941, 'XTylhuUlyB', 63), " +
            "(942, 'uOpdIMgqKE', 13), " +
            "(943, 'wrsORAOwwh', 13), " +
            "(944, 'RudRpihyhu', 66), " +
            "(945, 'fRqRmJPRFD', 24), " +
            "(946, 'pOdGNUISsO', 38), " +
            "(947, 'faqBfhXBbp', 42), " +
            "(948, 'owoGhHpexT', 23), " +
            "(949, 'BIZhhGMZpN', 25), " +
            "(950, 'aHILhIcMiE', 5), " +
            "(951, 'qrkXuDWKQb', 50), " +
            "(952, 'NvRHccpNvH', 47), " +
            "(953, 'vaCvQHZCqw', 64), " +
            "(954, 'ATAtitZsaL', 48), " +
            "(955, 'dXvXfRvrRd', 30), " +
            "(956, 'lbNuWgMGEf', 25), " +
            "(957, 'oivzltoNTG', 6), " +
            "(958, 'jtIGJawUJN', 47), " +
            "(959, 'EYsvKnZQBV', 5), " +
            "(960, 'RLFxALsehk', 22), " +
            "(961, 'VPGBlyOoxA', 62), " +
            "(962, 'kCUTHnKGjl', 29), " +
            "(963, 'nJmfNCkOoy', 63), " +
            "(964, 'wHgMNNUezg', 13), " +
            "(965, 'BsOYpTfjfi', 23), " +
            "(966, 'CWzqSJyefo', 12), " +
            "(967, 'TqbAxIFyiV', 4), " +
            "(968, 'VtFtRKLVeC', 4), " +
            "(969, 'NbkkekNyvo', 34), " +
            "(970, 'smvuMYDiIt', 4), " +
            "(971, 'TnOrMiDVdl', 23), " +
            "(972, 'MLHeulPhWO', 64), " +
            "(973, 'bCqDYjqpMq', 63), " +
            "(974, 'jUXTLlVThI', 49), " +
            "(975, 'SIuElhoYou', 68), " +
            "(976, 'AddeXVKbem', 57), " +
            "(977, 'jWFPfGojNw', 26), " +
            "(978, 'LjhBvXjpcN', 4), " +
            "(979, 'kwXbDwLmsN', 43), " +
            "(980, 'FmdMCHXcHh', 10), " +
            "(981, 'ZCaUENyItr', 67), " +
            "(982, 'JyseJmshVL', 63), " +
            "(983, 'gFhdVXvBNI', 43), " +
            "(984, 'wctoyIPjcx', 5), " +
            "(985, 'uzUratCHRu', 58), " +
            "(986, 'pxpwBBQSTE', 37), " +
            "(987, 'lhVZRnrbPn', 11), " +
            "(988, 'QUGcvndOuS', 19), " +
            "(989, 'cOqqDwKcub', 42), " +
            "(990, 'mcdjqKyJhl', 32), " +
            "(991, 'doemTazfIZ', 57), " +
            "(992, 'PhYyMPJmjK', 40), " +
            "(993, 'oxcAeCyWBk', 21), " +
            "(994, 'gECuhaRdin', 25), " +
            "(995, 'HjMzRKlnXy', 15), " +
            "(996, 'uUgcllEQja', 45), " +
            "(997, 'toULkletPN', 9), " +
            "(998, 'DmkjbDByCS', 12), " +
            "(999, 'HYXpcWIIoX', 49)"));
    }
}
