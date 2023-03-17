package com.github.jfsql.driver.cache;

import com.github.jfsql.parser.dto.BaseStatement;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import lombok.Getter;

@Getter
public class NullCacheImpl implements Cache {

    private final Map<String, BaseStatement> cachedStatements;

    public NullCacheImpl() {
        cachedStatements = new NullCacheMap<>();
    }

    static class NullCacheMap<K, V> implements Map<K, V> {

        @Override
        public int size() {
            return 0;
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public boolean containsKey(final Object key) {
            return false;
        }

        @Override
        public boolean containsValue(final Object value) {
            return false;
        }

        @Override
        public V get(final Object key) {
            return null;
        }

        @Override
        public V put(final K key, final V value) {
            return null;
        }

        @Override
        public V remove(final Object key) {
            return null;
        }

        @Override
        public void putAll(final Map<? extends K, ? extends V> m) {
            throw new UnsupportedOperationException("Not implemented on purpose.");
        }

        @Override
        public void clear() {
            throw new UnsupportedOperationException("Not implemented on purpose.");
        }

        @Override
        public Set<K> keySet() {
            return Collections.emptySet();
        }

        @Override
        public Collection<V> values() {
            return Collections.emptyList();
        }

        @Override
        public Set<Entry<K, V>> entrySet() {
            return Collections.emptySet();
        }
    }
}
