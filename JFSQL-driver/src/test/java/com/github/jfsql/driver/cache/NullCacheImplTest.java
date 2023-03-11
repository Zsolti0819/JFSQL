package com.github.jfsql.driver.cache;

import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.DeleteStatement;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;

class NullCacheImplTest {

    private final Cache cache = new NullCacheImpl();

    @Test
    void getCachedStatements() {
        final String sql = "DELETE FROM myTable";
        final DeleteStatement deleteStatement = (DeleteStatement) new Parser().parse(sql);
        cache.getCachedStatements().put(sql, deleteStatement);
        assertEquals(0, cache.getCachedStatements().size());
        assertTrue(cache.getCachedStatements().isEmpty());
        assertFalse(cache.getCachedStatements().containsKey(sql));
        assertFalse(cache.getCachedStatements().containsValue(deleteStatement));
        assertNull(cache.getCachedStatements().get(sql));
        assertNull(cache.getCachedStatements().put(sql, deleteStatement));
        assertNull(cache.getCachedStatements().remove(sql));
        assertThrows(Exception.class, () -> cache.getCachedStatements().putAll(Collections.emptyMap()));
        assertThrows(Exception.class, () -> cache.getCachedStatements().clear());
        assertEquals(Collections.emptySet(), cache.getCachedStatements().keySet());
        assertEquals(Collections.emptyList(), cache.getCachedStatements().values());
        assertEquals(Collections.emptySet(), cache.getCachedStatements().entrySet());
    }
}