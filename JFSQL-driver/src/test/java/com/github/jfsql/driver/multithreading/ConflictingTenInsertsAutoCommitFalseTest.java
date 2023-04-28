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
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.TestInstance;

/**
 * Parallel insert when there is conflict between tables. autoCommit is false. There are 10 insert per thread. 9 out of
 * 10 threads will be stopped due PessimisticLockException, and only one thread's inserts will be persisted and
 * committed.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class ConflictingTenInsertsAutoCommitFalseTest {

    private static final int NUM_THREADS = 10;

    @AfterEach
    void tearDown() {
        TestUtils.deleteDatabaseDirectory();
    }

    @RepeatedTest(100)
    void testConflictWhenInsertingTenToSameTable() throws Exception {
        final AtomicInteger pessimisticLocksCaught = new AtomicInteger();
        final Properties properties = new Properties();
        properties.setProperty("transaction.versioning", "default");
        try (final Connection tempConnection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH,
            properties)) {
            final Statement statement = tempConnection.createStatement();
            statement.execute("DROP TABLE IF EXISTS myTable");
            statement.execute("CREATE TABLE myTable (id TEXT, threadId TEXT)");
        }

        final Connection[] connections = new Connection[NUM_THREADS];
        for (int i = 0; i < NUM_THREADS; i++) {
            connections[i] = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH, properties);
        }

        // Spawn multiple threads to execute database operations
        final Thread[] threads = new Thread[NUM_THREADS];
        for (int i = 0; i < NUM_THREADS; i++) {
            final int index = i;
            threads[i] = new Thread(() -> {
                try {
                    final Connection connection = connections[index];
                    connection.setAutoCommit(false);
                    final String sql = "INSERT INTO myTable (id, threadId) VALUES (?, ?)";
                    final PreparedStatement preparedStatement = connection.prepareStatement(sql);
                    for (int j = 1; j <= 10; j++) {
                        preparedStatement.setInt(1, j);
                        preparedStatement.setLong(2, Thread.currentThread().getId());
                        preparedStatement.execute();
                        preparedStatement.close();
                    }
                    connection.commit();
                } catch (final SQLException e) {
                    e.printStackTrace();
                } catch (final PessimisticLockException pe) {
                    pessimisticLocksCaught.getAndIncrement();
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

        try (final Connection tempConnection = DriverManager.getConnection("jdbc:jfsql:" + TestUtils.DATABASE_PATH,
            properties)) {
            final Statement statement = tempConnection.createStatement();
            final JfsqlResultSet resultSet = (JfsqlResultSet) statement.executeQuery("SELECT * FROM myTable");
            final List<Entry> entries = resultSet.getEntries();
            assertEquals(10, entries.size());
        }
    }
}
