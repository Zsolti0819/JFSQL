package com.github.jfsql.driver.cache;

import com.github.jfsql.parser.dto.BaseStatement;
import java.util.Map;
import java.util.WeakHashMap;
import lombok.Getter;

@Getter
public class WeakCacheImpl implements Cache {

    private final Map<String, BaseStatement> cachedStatements;

    public WeakCacheImpl() {
        cachedStatements = new WeakHashMap<>();
    }
}
