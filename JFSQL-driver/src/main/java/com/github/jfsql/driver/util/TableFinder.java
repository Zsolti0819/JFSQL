package com.github.jfsql.driver.util;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class TableFinder {

    public Table getTableByName(final String tableName, final Database database) throws SQLException {
        final List<Table> tables = database.getTables();
        return tables.stream()
            .filter(t -> Objects.equals(tableName, t.getName()))
            .findFirst()
            .orElseThrow(() -> new SQLException("\"" + tableName + "\" not found"));
    }

}
