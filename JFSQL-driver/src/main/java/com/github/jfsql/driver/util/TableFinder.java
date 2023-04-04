package com.github.jfsql.driver.util;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class TableFinder {

    private final DatabaseManager databaseManager;

    public Table getTableByName(final String tableName) throws SQLException {
        final Database database = databaseManager.getDatabase();
        final List<Table> tables = database.getTables();
        return tables.stream()
            .filter(t -> Objects.equals(tableName, t.getName()))
            .findFirst()
            .orElseThrow(() -> new SQLException("\"" + tableName + "\" not found"));
    }

}
