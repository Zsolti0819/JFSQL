package com.github.jfsql.driver.multithreading;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlResultSet;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.exceptions.PessimisticLockException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Parallel insert when there is conflict between tables. autoCommit is false. 9 out of 10 threads will be stopped due
 * PessimisticLockException, and only one thread's insert will be persisted and committed.
 */
class ConflictingInsertsAutoCommitFalseTest {

    private static final int NUM_THREADS = 10;
    private static final int INSERT_COUNT = 10;

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @ValueSource(strings = {"jgit", "default"})
    void testConflictWhenInsertingOneToSameTable(final String transactionVersioning) throws Exception {
        final AtomicInteger pessimisticLocksCaught = new AtomicInteger();
        final Properties properties = new Properties();
        properties.setProperty("transaction.versioning", transactionVersioning);
        try (final Connection tempConnection = DriverManager.getConnection(TestUtils.URL, properties)) {
            final Statement statement = tempConnection.createStatement();
            statement.execute("DROP TABLE IF EXISTS myTable");
            statement.execute("CREATE TABLE myTable (id TEXT, threadId TEXT)");
        }

        final Connection[] connections = new Connection[NUM_THREADS];
        for (int i = 0; i < NUM_THREADS; i++) {
            connections[i] = DriverManager.getConnection(TestUtils.URL, properties);
        }

        // Create a CountDownLatch with a count of NUM_THREADS
        final CountDownLatch latch = new CountDownLatch(NUM_THREADS);

        // Spawn multiple threads to execute database operations
        final Thread[] threads = new Thread[NUM_THREADS];
        for (int i = 0; i < NUM_THREADS; i++) {
            final int index = i;
            threads[i] = new Thread(() -> {
                try {
                    latch.countDown();
                    latch.await();
                    connections[index].setAutoCommit(false);
                    final String sql = "INSERT INTO myTable (id, threadId) VALUES (?, ?)";
                    final PreparedStatement preparedStatement = connections[index].prepareStatement(sql);
                    for (int j = 0; j < INSERT_COUNT; j++) {
                        preparedStatement.setInt(1, 1);
                        preparedStatement.setLong(2, Thread.currentThread().getId());
                        preparedStatement.execute();
                    }
                    connections[index].commit();
                    preparedStatement.close();
                } catch (final SQLException e) {
                    e.printStackTrace();
                } catch (final PessimisticLockException pe) {
                    pessimisticLocksCaught.getAndIncrement();
                } catch (final InterruptedException ie) {
                    Thread.currentThread().interrupt();
                }
            });
        }

        // Start all threads
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

        assertEquals(NUM_THREADS - 1, pessimisticLocksCaught.get());

        try (final Connection tempConnection = DriverManager.getConnection(TestUtils.URL, properties)) {
            final Statement statement = tempConnection.createStatement();
            final JfsqlResultSet resultSet = (JfsqlResultSet) statement.executeQuery("SELECT * FROM myTable");
            final List<Entry> entries = resultSet.getEntries();
            assertEquals(INSERT_COUNT, entries.size());
        }
    }
}
