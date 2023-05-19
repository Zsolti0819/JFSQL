package com.github.jfsql.driver.jdbc;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.jfsql.driver.TestUtils;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class SelectTest {

    private Statement statement;
    private Connection connection;

    private void setup(final String persistence, final String transactionVersioning) throws SQLException {
        final Properties properties = new Properties();
        properties.setProperty("persistence", persistence);
        properties.setProperty("transaction.versioning", transactionVersioning);
        properties.setProperty("statement.caching", "true");
        properties.setProperty("schema.validation", "true");
        connection = DriverManager.getConnection(TestUtils.URL, properties);
        statement = connection.createStatement();
        statement.execute("DROP TABLE IF EXISTS myTable");
        statement.execute("DROP TABLE IF EXISTS myTable2");
        statement.execute("CREATE TABLE myTable (id INTEGER, name TEXT, age INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable (id, name, age) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 34)");
        statement.execute("CREATE TABLE myTable2 (id INTEGER , name2 TEXT, age2 INTEGER)");
        statement.executeUpdate(
            "INSERT INTO myTable2 (id, name2, age2) VALUES (1, 'Zsolti', 25), (2, 'Tomi', 24), (3, 'Ivan', 26), (4, 'Lukas', 1)");
    }

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    /**
     * <a href="https://www.sqlshack.com/sql-multiple-joins-for-beginners-with-examples/">Based on this tutorial</a>
     */
    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_multipleJoin(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        statement.execute("DROP TABLE IF EXISTS sales");
        statement.execute("DROP TABLE IF EXISTS orders");
        statement.execute("DROP TABLE IF EXISTS onlineCustomers");
        statement.execute(
            "CREATE TABLE onlineCustomers (customerId INTEGER, customerName TEXT, customerCity TEXT, customerMail TEXT)");
        statement.execute(
            "CREATE TABLE orders (orderId INTEGER, customerId INTEGER, orderTotal REAL, discountRate REAL, orderDate TEXT)");
        statement.execute("CREATE TABLE sales (salesId INTEGER, orderId INTEGER, salesTotal REAL)");
        statement.execute(
            "INSERT INTO onlineCustomers VALUES (1, 'Salvador', 'Philadelphia', 'tyiptqo.wethls@chttw.org')");
        statement.execute("INSERT INTO onlineCustomers VALUES (2, 'Gilbert', 'San Diego', 'rrvyy.wdumos@lklkj.org')");
        statement.execute("INSERT INTO onlineCustomers VALUES (3, 'Ernest', 'New York', 'ymuea.pnxkukf@dwv.org')");
        statement.execute("INSERT INTO onlineCustomers VALUES (4, 'Stella', 'Phoenix', 'xvsfzp.rjhtni@rdn.com')");
        statement.execute("INSERT INTO onlineCustomers VALUES (5, 'Jorge', 'Los Angeles', 'oykbo.vlxopp@nmwhv.org')");
        statement.execute("INSERT INTO onlineCustomers VALUES (6, 'Jerome', 'San Antonio', 'wkabc.ofmhetq@gtmh.co')");
        statement.execute("INSERT INTO onlineCustomers VALUES (7, 'Edward', 'Chicago', 'wguexiymy.nnbdgpc@juc.co')");
        statement.execute(
            "INSERT INTO orders (orderId, customerId, orderTotal, discountRate, orderDate) VALUES (1, 3, 1910.64,5.49, '03-Dec-2019')");
        statement.execute(
            "INSERT INTO orders (orderId, customerId, orderTotal, discountRate, orderDate) VALUES (2, 4, 150.89,15.33, '11-Jun-2019')");
        statement.execute(
            "INSERT INTO orders (orderId, customerId, orderTotal, discountRate, orderDate) VALUES (3, 5, 912.55,13.74, '15-Sep-2019')");
        statement.execute(
            "INSERT INTO orders (orderId, customerId, orderTotal, discountRate, orderDate) VALUES (4, 7, 418.24,14.53, '28-May-2019')");
        statement.execute(
            "INSERT INTO orders (orderId, customerId, orderTotal, discountRate, orderDate) VALUES (5, 55 ,512.55,13.74, '15-Jun-2019')");
        statement.execute(
            "INSERT INTO orders (orderId, customerId, orderTotal, discountRate, orderDate) VALUES (6, 57, 118.24,14.53, '28-Dec-2019')");
        statement.execute("INSERT INTO sales (salesId, orderId, salesTotal) VALUES(1, 3, 370.95)");
        statement.execute("INSERT INTO sales (salesId, orderId, salesTotal) VALUES(2, 4, 882.13)");
        statement.execute("INSERT INTO sales (salesId, orderId, salesTotal) VALUES(3, 12, 370.95)");
        statement.execute("INSERT INTO sales (salesId, orderId, salesTotal) VALUES(4, 13, 882.13)");
        statement.execute("INSERT INTO sales (salesId, orderId, salesTotal) VALUES(5, 55, 170.95)");
        statement.execute("INSERT INTO sales (salesId, orderId, salesTotal) VALUES(6, 57, 382.13)");
        statement.execute(
            "SELECT customerName, customerCity, customerMail, salesTotal FROM onlineCustomers INNER JOIN orders ON onlineCustomers.customerId = orders.customerId INNER JOIN sales ON orders.orderId = sales.orderId ORDER BY onlineCustomers.customerId ASC");

        final List<String> customerNames = new ArrayList<>();
        final List<String> customerCities = new ArrayList<>();
        final List<String> customerMails = new ArrayList<>();
        final List<Double> salesTotals = new ArrayList<>();

        final ResultSet resultSet = statement.getResultSet();
        while (resultSet.next()) {
            customerNames.add(resultSet.getString("customerName"));
            customerCities.add(resultSet.getString("customerCity"));
            customerMails.add(resultSet.getString("customerMail"));
            salesTotals.add(resultSet.getDouble("salesTotal"));
        }

        final String[] expectedCustomerNames = {"Jorge", "Edward"};
        final String[] expectedCustomerCities = {"Los Angeles", "Chicago"};
        final String[] expectedCustomerMails = {"oykbo.vlxopp@nmwhv.org", "wguexiymy.nnbdgpc@juc.co"};
        final Double[] expectedSalesTotals = {370.95, 882.13};

        assertArrayEquals(expectedCustomerNames, customerNames.toArray());
        assertArrayEquals(expectedCustomerCities, customerCities.toArray());
        assertArrayEquals(expectedCustomerMails, customerMails.toArray());
        assertArrayEquals(expectedSalesTotals, salesTotals.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_innerJoin(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        assertTrue(statement.execute("SELECT * FROM myTable JOIN myTable2 ON myTable2.age2 = myTable.id"));
        final List<Integer> ids = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ages = new ArrayList<>();
        final List<String> names2 = new ArrayList<>();
        final List<Integer> ages2 = new ArrayList<>();

        final ResultSet resultSet = statement.getResultSet();
        while (resultSet.next()) {
            ids.add(resultSet.getInt("myTable.id"));
            names.add(resultSet.getString("name"));
            ages.add(resultSet.getInt("age"));
            names2.add(resultSet.getString("name2"));
            ages2.add(resultSet.getInt("age2"));
        }
        final Integer[] expectedIdsArray = {1};
        final String[] expectedNamesArray = {"Zsolti"};
        final Integer[] expectedAgesArray = {25};
        final String[] expectedNames2Array = {"Lukas"};
        final Integer[] expectedAges2Array = {1};

        assertArrayEquals(expectedIdsArray, ids.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNames2Array, names2.toArray());
        assertArrayEquals(expectedAges2Array, ages2.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_leftJoin(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        assertTrue(statement.execute("SELECT * FROM myTable LEFT JOIN myTable2 ON myTable.id = myTable2.age2"));
        final List<Integer> ids = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ages = new ArrayList<>();
        final List<String> names2 = new ArrayList<>();
        final List<Integer> ages2 = new ArrayList<>();

        final ResultSet resultSet = statement.getResultSet();
        while (resultSet.next()) {
            ids.add(resultSet.getInt("myTable.id"));
            names.add(resultSet.getString("name"));
            ages.add(resultSet.getInt("age"));
            names2.add(resultSet.getString("name2"));
            ages2.add(resultSet.getInt("age2"));
        }
        final Integer[] expectedIdsArray = {1, 2, 3, 4};
        final String[] expectedNamesArray = {"Zsolti", "Tomi", "Ivan", "Lukas"};
        final Integer[] expectedAgesArray = {25, 24, 26, 34};
        final String[] expectedNames2Array = {"Lukas", null, null, null};
        final Integer[] expectedAges2Array = {1, 0, 0, 0};

        assertArrayEquals(expectedIdsArray, ids.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNames2Array, names2.toArray());
        assertArrayEquals(expectedAges2Array, ages2.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_all(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        statement.execute("SELECT * FROM myTable");
        final ResultSet resultSet = statement.getResultSet();
        while (resultSet.next()) {
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final String[] expectedNamesArray = {"Zsolti", "Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {1, 2, 3, 4};

        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_whereIntegerGt(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final ResultSet resultSet = statement.executeQuery("SELECT age, name, id FROM myTable WHERE id > 1");
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {24, 26, 34};
        final String[] expectedNamesArray = {"Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {2, 3, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_whereLike(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final ResultSet resultSet = statement.executeQuery(
            "SELECT age, name, id FROM myTable WHERE name LIKE 'Z_olti' OR name LIKE '%uka%'");
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {25, 34};
        final String[] expectedNamesArray = {"Zsolti", "Lukas"};
        final Integer[] expectedIdsArray = {1, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_whereMultipleANDs(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final ResultSet resultSet = statement.executeQuery(
            "SELECT age, name, id FROM myTable WHERE id > 1 AND age >= 24 AND name = 'Lukas'");
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {34};
        final String[] expectedNamesArray = {"Lukas"};
        final Integer[] expectedIdsArray = {4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_whereIntegerGte(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final ResultSet resultSet = statement.executeQuery("SELECT age, name, id FROM myTable WHERE id >= 2");
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {24, 26, 34};
        final String[] expectedNamesArray = {"Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {2, 3, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_columnsByIndex(final String persistence, final String transactionVersioning) throws SQLException {
        setup(persistence, transactionVersioning);

        final ResultSet resultSet = statement.executeQuery("SELECT id, age, name FROM myTable WHERE id=1");
        while (resultSet.next()) {
            assertEquals("Zsolti", resultSet.getString(3));
            assertEquals(25, resultSet.getInt(2));
            assertEquals(1, resultSet.getInt(1));
        }
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_columnsByColumnName(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final ResultSet resultSet = statement.executeQuery("SELECT name, id, age FROM myTable WHERE id=1");
        while (resultSet.next()) {
            assertEquals(25, resultSet.getInt("age"));
            assertEquals("Zsolti", resultSet.getString("name"));
            assertEquals(1, resultSet.getInt("id"));
        }
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_notExistingColumn(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeQuery("SELECT lol FROM myTable;"));
        assertEquals("Column 'lol' not found in table 'myTable'.", thrown.getMessage());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_notExistingColumnInJoinedTable(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final SQLException thrown = assertThrows(SQLException.class,
            () -> statement.executeQuery("SELECT lol FROM myTable JOIN myTable2 ON myTable2.age2 = myTable.id"));
        assertEquals("Column 'lol' not found in the joined table.", thrown.getMessage());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_resultSetMetadata(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final ResultSet resultSet = statement.executeQuery("SELECT * FROM myTable");
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        assertEquals(3, resultSetMetaData.getColumnCount());
        assertEquals("id", resultSetMetaData.getColumnName(1));
        assertEquals("name", resultSetMetaData.getColumnName(2));
        assertEquals("age", resultSetMetaData.getColumnName(3));
        assertTrue(resultSetMetaData.isSearchable(3));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_resultSetMetadataAllManual(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final ResultSet resultSet = statement.executeQuery("SELECT age, name, id FROM myTable");
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        assertEquals(3, resultSetMetaData.getColumnCount());
        assertEquals("id", resultSetMetaData.getColumnName(3));
        assertEquals("name", resultSetMetaData.getColumnName(2));
        assertEquals("age", resultSetMetaData.getColumnName(1));
        assertTrue(resultSetMetaData.isSearchable(3));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_resultSetMetadataNotAll(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final ResultSet resultSet = statement.executeQuery("SELECT id, age FROM myTable");
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        assertEquals(2, resultSetMetaData.getColumnCount());
        assertEquals("id", resultSetMetaData.getColumnName(1));
        assertEquals("age", resultSetMetaData.getColumnName(2));
        assertTrue(resultSetMetaData.isSearchable(2));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_resultSetMetadataOne(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final ResultSet resultSet = statement.executeQuery("SELECT name FROM myTable");
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        assertEquals(1, resultSetMetaData.getColumnCount());
        assertEquals("name", resultSetMetaData.getColumnName(1));
        assertTrue(resultSetMetaData.isSearchable(1));
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_preparedStatement_whereIntegerGt(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE id > 1");
        preparedStatement.setInt(1, 1);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {24, 26, 34};
        final String[] expectedNamesArray = {"Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {2, 3, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_preparedStatement_whereLike(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE name LIKE ? OR name LIKE ?");
        preparedStatement.setString(1, "Z_olti");
        preparedStatement.setString(2, "%uka%");
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {25, 34};
        final String[] expectedNamesArray = {"Zsolti", "Lukas"};
        final Integer[] expectedIdsArray = {1, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_preparedStatement_whereMultipleANDs(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE id > ? AND age >= ? AND name = ?");
        preparedStatement.setInt(1, 1);
        preparedStatement.setInt(2, 24);
        preparedStatement.setString(3, "Lukas");
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {34};
        final String[] expectedNamesArray = {"Lukas"};
        final Integer[] expectedIdsArray = {4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_preparedStatement_whereIntegerGte(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final List<Integer> ages = new ArrayList<>();
        final List<String> names = new ArrayList<>();
        final List<Integer> ids = new ArrayList<>();
        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT age, name, id FROM myTable WHERE id >= ?");
        preparedStatement.setInt(1, 2);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            ages.add(resultSet.getInt("age"));
            names.add(resultSet.getString("name"));
            ids.add(resultSet.getInt("id"));
        }

        final Integer[] expectedAgesArray = {24, 26, 34};
        final String[] expectedNamesArray = {"Tomi", "Ivan", "Lukas"};
        final Integer[] expectedIdsArray = {2, 3, 4};

        assertArrayEquals(expectedAgesArray, ages.toArray());
        assertArrayEquals(expectedNamesArray, names.toArray());
        assertArrayEquals(expectedIdsArray, ids.toArray());
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_preparedStatement_columnsByIndex(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT id, age, name FROM myTable WHERE id = ?");
        preparedStatement.setInt(1, 1);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            assertEquals("Zsolti", resultSet.getString(3));
            assertEquals(25, resultSet.getInt(2));
            assertEquals(1, resultSet.getInt(1));
        }
    }

    @ParameterizedTest
    @MethodSource("com.github.jfsql.driver.jdbc.TestConfiguration#configurations")
    void testSelect_preparedStatement_columnsByColumnName(final String persistence, final String transactionVersioning)
        throws SQLException {
        setup(persistence, transactionVersioning);

        final PreparedStatement preparedStatement = connection.prepareStatement(
            "SELECT id, age, name FROM myTable WHERE id = ?");
        preparedStatement.setInt(1, 1);
        final ResultSet resultSet = preparedStatement.executeQuery();
        while (resultSet.next()) {
            assertEquals(25, resultSet.getInt("age"));
            assertEquals("Zsolti", resultSet.getString("name"));
            assertEquals(1, resultSet.getInt("id"));
        }
    }

}
