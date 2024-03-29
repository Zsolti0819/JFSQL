package com.github.jfsql.driver.cache.resultset;

import com.github.jfsql.driver.core.JfsqlResultSet;
import com.github.jfsql.parser.dto.SelectWrapper;
import java.sql.ResultSet;
import java.util.Map;
import java.util.Objects;
import java.util.WeakHashMap;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ResultSetCache {

    public static final Map<SelectWrapper, ResultSet> CACHED_RESULT_SETS = new WeakHashMap<>();
    private final Object lock = new Object();

    public void removeResultSetFromCache(final String tableName) {
        synchronized (lock) {
            CACHED_RESULT_SETS.entrySet().removeIf(entry -> {
                    final SelectWrapper statement = entry.getKey();
                    return Objects.equals(statement.getTableName(), tableName) ||
                        statement.getJoinTableNames().contains(tableName);
                }
            );
        }
    }

    public void addResultSetToCache(final SelectWrapper statement, final ResultSet resultSet) {
        synchronized (lock) {
            CACHED_RESULT_SETS.put(statement, resultSet);
        }
    }

    public ResultSet getResultSetFromCache(final SelectWrapper statement) {
        synchronized (lock) {
            final JfsqlResultSet resultSet = (JfsqlResultSet) CACHED_RESULT_SETS.get(statement);
            resultSet.setCurrentEntry(0);
            return resultSet;
        }
    }

}
