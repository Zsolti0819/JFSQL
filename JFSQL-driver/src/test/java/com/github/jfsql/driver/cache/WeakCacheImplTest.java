package com.github.jfsql.driver.cache;

import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.DeleteStatement;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

class WeakCacheImplTest {

    private final Cache cache = new WeakCacheImpl();

    @Test
    void getCachedStatements() {
        final String sql = "DELETE FROM myTable";
        final DeleteStatement deleteStatement = (DeleteStatement) new Parser().parse(sql);
        cache.getCachedStatements().put(sql, deleteStatement);
        assertTrue(cache.getCachedStatements().containsKey(sql));
    }
}
