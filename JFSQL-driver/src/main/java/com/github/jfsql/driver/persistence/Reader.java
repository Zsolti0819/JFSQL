package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import java.io.File;
import java.sql.SQLException;
import java.util.List;
import java.util.Set;

public interface Reader {

    String getFileExtension();

    String getSchemaFileExtension();

    List<Entry> readEntriesFromTable(final Table table) throws SQLException;

    Table readSchema(final String pathToSchema) throws SQLException;

    List<Table> readTablesFromDatabaseFile(final Database database) throws SQLException;

    String readBlob(final String pathToBlob) throws SQLException;

    Set<File> getFilesFromDatabaseFile(final Database database) throws SQLException;

    Set<File> getBlobsFromTables(final Database database) throws SQLException;

}
