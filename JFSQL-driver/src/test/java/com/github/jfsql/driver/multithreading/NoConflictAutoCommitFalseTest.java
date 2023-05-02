package com.github.jfsql.driver.multithreading;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlResultSet;
import com.github.jfsql.driver.dto.Entry;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Parallel inserts when there is no conflict between tables. autoCommit is false. No exceptions are expected. Each
 * thread inserts to different tables, 10 threads - 10 tables
 */
class NoConflictAutoCommitFalseTest {

    private static final int NUM_THREADS = 10;
    private static final int INSERT_COUNT = 10;

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @ValueSource(strings = {"jgit", "default"})
    void testParallelInsertsWithDifferentTables(final String transactionVersioning) throws Exception {
        final Properties properties = new Properties();
        properties.setProperty("transaction.versioning", transactionVersioning);
        final Connection[] connections = new Connection[NUM_THREADS];
        final Statement[] statements = new Statement[NUM_THREADS];
        final String[] tableNames = new String[NUM_THREADS];

        for (int i = 0; i < NUM_THREADS; i++) {
            connections[i] = DriverManager.getConnection(TestUtils.URL, properties);
            statements[i] = connections[i].createStatement();
            tableNames[i] = "myTable" + i;
            statements[i].execute("DROP TABLE IF EXISTS " + tableNames[i]);
            statements[i].execute("CREATE TABLE " + tableNames[i] + "(id TEXT, threadId TEXT)");
        }

        // Create a CountDownLatch with a count of NUM_THREADS
        final CountDownLatch latch = new CountDownLatch(NUM_THREADS);

        // Spawn multiple threads to execute database operations
        final Thread[] threads = new Thread[NUM_THREADS];
        for (int i = 0; i < NUM_THREADS; i++) {
            final int threadIndex = i;
            threads[i] = new Thread(() -> {
                try (final Connection connection = connections[threadIndex]) {
                    // Count down the latch before executing the insert
                    latch.countDown();
                    latch.await(); // Wait for all threads to count down the latch
                    connection.setAutoCommit(false);
                    final String sql = "INSERT INTO " + tableNames[threadIndex] + "(id, threadId) VALUES (?, ?)";
                    final PreparedStatement preparedStatement = connection.prepareStatement(sql);
                    for (int j = 0; j < INSERT_COUNT; j++) {
                        preparedStatement.setInt(1, j);
                        preparedStatement.setLong(2, Thread.currentThread().getId());
                        preparedStatement.executeUpdate();
                    }
                    connection.commit();
                } catch (final SQLException e) {
                    e.printStackTrace();
                } catch (final InterruptedException ie) {
                    Thread.currentThread().interrupt();
                }
            });
        }

        // Wait for all threads to finish
        for (final Thread thread : threads) {
            thread.start();
        }

        // Wait for all threads to finish
        for (final Thread thread : threads) {
            thread.join();
        }

        // Close the database connections
        for (final Connection conn : connections) {
            conn.close();
        }

        try (final Connection tempConnection = DriverManager.getConnection(TestUtils.URL, properties)) {
            final Statement statement = tempConnection.createStatement();
            for (int i = 0; i < NUM_THREADS; i++) {
                final JfsqlResultSet resultSet = (JfsqlResultSet) statement.executeQuery(
                    "SELECT * FROM " + tableNames[i]);
                final List<Entry> entries = resultSet.getEntries();
                assertEquals(INSERT_COUNT, entries.size());
            }
        }
    }
}
