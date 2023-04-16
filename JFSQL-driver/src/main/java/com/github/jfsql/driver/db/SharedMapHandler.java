package com.github.jfsql.driver.db;

import static com.github.jfsql.driver.core.JfsqlDriver.OBJECT_NAME_TO_THREAD_ID_MAP;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.PessimisticLockException;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class SharedMapHandler {


    public void addTableToSharedMap(final Table table) {
        synchronized (OBJECT_NAME_TO_THREAD_ID_MAP) {
            if (OBJECT_NAME_TO_THREAD_ID_MAP.containsKey(table.getName())) {
                if (!Objects.equals(OBJECT_NAME_TO_THREAD_ID_MAP.get(table.getName()),
                    Thread.currentThread().getId())) {
                    // remove all entries from the shared map, where the value was the thread's id
                    removeCurrentThreadChangesFromMap();
                    throw new PessimisticLockException(
                        "The file '" + table.getTableFile() + "' is currently being modified by another thread.");
                }
            } else {
                OBJECT_NAME_TO_THREAD_ID_MAP.put(table.getName(), Thread.currentThread().getId());
            }
        }
    }

    public void addSchemaToSharedMap(final Table table) {
        synchronized (OBJECT_NAME_TO_THREAD_ID_MAP) {
            if (OBJECT_NAME_TO_THREAD_ID_MAP.containsKey(table.getName())) {
                if (!Objects.equals(OBJECT_NAME_TO_THREAD_ID_MAP.get(table.getName()),
                    Thread.currentThread().getId())) {
                    // remove all entries from the shared map, where the value was the thread's id
                    removeCurrentThreadChangesFromMap();
                    throw new PessimisticLockException(
                        "The file '" + table.getSchemaFile() + "' is currently being modified by another thread.");
                }
            } else {
                OBJECT_NAME_TO_THREAD_ID_MAP.put(table.getName(), Thread.currentThread().getId());
            }
        }
    }

    public void addDatabaseToSharedMap(final Database database) {
        synchronized (OBJECT_NAME_TO_THREAD_ID_MAP) {
            if (OBJECT_NAME_TO_THREAD_ID_MAP.containsKey(database.getName())) {
                if (!Objects.equals(OBJECT_NAME_TO_THREAD_ID_MAP.get(database.getName()),
                    Thread.currentThread().getId())) {
                    // remove all entries from the shared map, where the value was the thread's id
                    removeCurrentThreadChangesFromMap();
                    throw new PessimisticLockException(
                        "The file '" + database.getURL() + "' is currently being modified by another thread.");
                }
            } else {
                OBJECT_NAME_TO_THREAD_ID_MAP.put(database.getName(), Thread.currentThread().getId());
            }
        }
    }

    public void removeCurrentThreadChangesFromMap() {
        synchronized (OBJECT_NAME_TO_THREAD_ID_MAP) {
            final Iterator<Entry<String, Long>> iterator = OBJECT_NAME_TO_THREAD_ID_MAP.entrySet().iterator();
            while (iterator.hasNext()) {
                final Map.Entry<String, Long> entry = iterator.next();
                final Long value = entry.getValue();
                if (Objects.equals(value, Thread.currentThread().getId())) {
                    iterator.remove();
                }
            }
        }
    }

}
