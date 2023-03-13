package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import lombok.RequiredArgsConstructor;

import java.sql.SQLException;
import java.util.Objects;

@RequiredArgsConstructor
public class TableFinder {

    private final Database database;

    public Table getTableByName(final String tableName) throws SQLException {
        return database.getTables().stream()
                .filter(t -> Objects.equals(tableName, t.getName()))
                .findFirst()
                .orElseThrow(() -> new SQLException("\"" + tableName + "\" not found"));
    }

}
