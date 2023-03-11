package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.cache.NullCacheImpl;
import com.github.jfsql.driver.cache.WeakCacheImpl;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class CacheFactory {

    public Cache createCache(final String type) {
        if (Objects.equals("true", type)) {
            return new WeakCacheImpl();
        } else if (Objects.equals("false", type)) {
            return new NullCacheImpl();
        } else {
            throw new IllegalArgumentException("Unknown Cache type");
        }
    }
}
