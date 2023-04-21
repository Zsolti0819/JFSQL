package com.github.jfsql.driver.cache.resultset;

import com.github.jfsql.parser.dto.SelectWrapper;
import java.sql.ResultSet;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ResultSetCache {

    public static final Map<SelectWrapper, ResultSet> CACHED_RESULT_SETS = new ConcurrentHashMap<>();

    public synchronized void removeResultSetFromCache(final String tableName) {
        CACHED_RESULT_SETS.keySet().stream()
            .filter(selectWrapper -> Objects.equals(selectWrapper.getTableName(), tableName) ||
                selectWrapper.getJoinTableNames().contains(tableName)).forEach(CACHED_RESULT_SETS::remove);
    }

}
