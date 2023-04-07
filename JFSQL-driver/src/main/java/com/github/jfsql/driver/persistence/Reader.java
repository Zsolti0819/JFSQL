package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Set;

public interface Reader {

    String getFileExtension();

    String getSchemaFileExtension();

    List<Entry> readEntriesFromTable(final Table table) throws IOException;

    Table readSchema(final String pathToSchema) throws IOException;

    List<Table> readTablesFromDatabaseFile(final Database database) throws IOException;

    String readBlob(final String pathToBlob) throws IOException;

    Set<File> getFilesFromDatabaseFile(final Database database) throws IOException;

    Set<File> getBlobsFromTables(final Database database) throws IOException;

}
