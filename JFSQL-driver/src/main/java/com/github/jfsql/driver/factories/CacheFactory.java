package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.cache.statement.Cache;
import com.github.jfsql.driver.cache.statement.NullCacheImpl;
import com.github.jfsql.driver.cache.statement.WeakCacheImpl;
import com.github.jfsql.driver.config.PropertiesReader;
import lombok.experimental.UtilityClass;

@UtilityClass
public class CacheFactory {

    public Cache createCache(final PropertiesReader propertiesReader) {
        final boolean useCaching = propertiesReader.isStatementCaching();
        if (useCaching) {
            return new WeakCacheImpl();
        } else {
            return new NullCacheImpl();
        }
    }
}
