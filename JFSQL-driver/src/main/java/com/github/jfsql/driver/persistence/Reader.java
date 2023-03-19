package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Schema;
import com.github.jfsql.driver.dto.Table;
import java.sql.SQLException;
import java.util.List;

public interface Reader {

    String getFileExtension();

    String getSchemaFileExtension();

    List<Entry> readEntriesFromTable(final Table table) throws SQLException;

    Schema readSchema(final String pathToSchema) throws SQLException;

    List<Table> readTablesFromDatabaseFile(final Database database) throws SQLException;

    String readBlob(final String pathToBlob) throws SQLException;

    boolean pathIsPresentInDatabaseFile(final Database database, final String pathToCheck) throws SQLException;

}
