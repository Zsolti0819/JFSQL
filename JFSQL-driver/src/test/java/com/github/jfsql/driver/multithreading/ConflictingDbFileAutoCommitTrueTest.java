package com.github.jfsql.driver.multithreading;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.core.JfsqlConnection;
import com.github.jfsql.driver.exceptions.PessimisticLockException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * NUM_THREADS threads try to modify the database file simultaneously. Only one thread's statement will succeed, the
 * other will get PessimisticLockException.
 */
class ConflictingDbFileAutoCommitTrueTest {

    private static final int NUM_THREADS = 10;

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @ParameterizedTest
    @ValueSource(strings = {"jgit", "default"})
    void testConflictWhenCreatingTables(final String transactionVersioning) throws Exception {
        final AtomicInteger pessimisticLocksCaught = new AtomicInteger();
        final Properties properties = new Properties();
        properties.setProperty("transaction.versioning", transactionVersioning);
        try (final Connection tempConnection = DriverManager.getConnection(TestUtils.URL, properties)) {
            final Statement statement = tempConnection.createStatement();
            statement.execute("DROP TABLE IF EXISTS myTable");
            statement.execute("DROP TABLE IF EXISTS myTable2");
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
            final int finalI = i;
            threads[finalI] = new Thread(() -> {
                try {
                    latch.countDown();
                    latch.await();
                    final Statement statement = connections[finalI].createStatement();
                    statement.execute("CREATE TABLE myTable" + finalI + " (id INTEGER)");
                } catch (final SQLException e) {
                    e.printStackTrace();
                } catch (final PessimisticLockException pe) {
                    pessimisticLocksCaught.getAndIncrement();
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

        assertEquals(NUM_THREADS - 1, pessimisticLocksCaught.get());

        try (final JfsqlConnection tempConnection = (JfsqlConnection) DriverManager.getConnection(TestUtils.URL,
            properties)) {
            final int tableCount = tempConnection.getDatabaseManager().getDatabase().getTables().size();
            assertEquals(1, tableCount);
        }
    }
}
