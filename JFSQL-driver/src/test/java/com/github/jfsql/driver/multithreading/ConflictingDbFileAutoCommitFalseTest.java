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
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.TestInstance;

/**
 * Two threads try to modify the database file simultaneously. Only one thread's statement will succeed, the other will
 * get PessimisticLockException.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class ConflictingDbFileAutoCommitFalseTest {

    private static final int NUM_THREADS = 2;

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @RepeatedTest(10)
    public void testConflictWhenCreatingTables() throws Exception {
        final AtomicInteger pessimisticLocksCaught = new AtomicInteger();
        final Properties properties = new Properties();
        properties.setProperty("transaction.versioning", "default");
        try (final Connection tempConnection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH,
            properties)) {
            final Statement statement = tempConnection.createStatement();
            statement.execute("DROP TABLE IF EXISTS myTable");
            statement.execute("DROP TABLE IF EXISTS myTable2");
        }

        final Connection[] connections = new Connection[NUM_THREADS];
        for (int i = 0; i < NUM_THREADS; i++) {
            connections[i] = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        }

        // Spawn multiple threads to execute database operations
        final Thread[] threads = new Thread[NUM_THREADS];
        threads[0] = new Thread(() -> {
            try {
                connections[0].setAutoCommit(false);
                final Statement statement = connections[0].createStatement();
                statement.execute("CREATE TABLE myTable (id INTEGER)");
                connections[0].commit();
            } catch (final SQLException e) {
                e.printStackTrace();
            } catch (final PessimisticLockException pe) {
                pessimisticLocksCaught.getAndIncrement();
            }
        });

        threads[1] = new Thread(() -> {
            try {
                connections[1].setAutoCommit(false);
                final Statement statement = connections[1].createStatement();
                statement.execute("CREATE TABLE myTable2 (id INTEGER)");
                connections[1].commit();
            } catch (final SQLException e) {
                e.printStackTrace();
            } catch (final PessimisticLockException pe) {
                pessimisticLocksCaught.getAndIncrement();
            }
        });

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

        assertEquals(1, pessimisticLocksCaught.get());

        try (final JfsqlConnection tempConnection = (JfsqlConnection) DriverManager.getConnection(
            "jdbc:jfsql:" + TestUtils.DATABASE_PATH,
            properties)) {
            final int tableCount = tempConnection.getDatabaseManager().getDatabase().getTables().size();
            assertEquals(1, tableCount);
        }
    }
}
