package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.PropertiesReader;
import lombok.Getter;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Getter
public abstract class Writer {

    static final Boolean USE_SCHEMA_VALIDATION = Boolean.valueOf(PropertiesReader.getProperty("schema.validation"));
    private final List<Table> uncommittedTables;
    private final List<Table> uncommittedSchemas;
    private final List<Database> uncommittedDatabases;

    protected Writer() {
        uncommittedTables = new ArrayList<>();
        uncommittedSchemas = new ArrayList<>();
        uncommittedDatabases = new ArrayList<>();
    }

    public abstract String getFileExtension();

    public abstract String getSchemaFileExtension();

    // Tables

    public void addTableToUncommittedObjects(final Table table) {
        uncommittedTables.removeIf(t -> Objects.equals(t.getName(), table.getName()));
        uncommittedTables.add(table);
    }

    public void writeUncommittedTables() throws SQLException {
        for (final Table table : uncommittedTables) {
            writeTable(table);
        }
    }

    public abstract void writeTable(final Table table) throws SQLException;

    // Schemas

    public void addSchemaToUncommittedObjects(final Table table) {
        uncommittedSchemas.removeIf(t -> Objects.equals(t.getName(), table.getName()));
        uncommittedSchemas.add(table);
    }

    public void writeUncommittedSchemas() throws SQLException {
        for (final Table table : uncommittedSchemas) {
            writeSchema(table);
        }
    }

    public abstract void writeSchema(final Table table) throws SQLException;

    // Database Files

    public void addDatabaseToUncommittedObjects(final Database database) {
        uncommittedDatabases.removeIf(db -> Objects.equals(db.getUrl(), database.getUrl()));
        uncommittedDatabases.add(database);
    }

    public void writeUncommittedDatabases() throws SQLException {
        for (final Database database : uncommittedDatabases) {
            writeDatabaseFile(database);
        }
    }

    public abstract void writeDatabaseFile(final Database database) throws SQLException;

    // LOBs

    abstract Path writeBlob(final Table table, final String value) throws SQLException;

    String incrementFileName(final Path folderPath, final String fileExtension) {
        final List<Integer> fileNumbers = new ArrayList<>();
        final Pattern pattern = Pattern.compile("blob" + "(\\d+)" + "\\." + fileExtension);
        final Collection<File> filesInFolder = FileUtils.listFiles(folderPath.toFile(),
                new String[]{getFileExtension()}, false);
        for (final File file : filesInFolder) {
            final String fileName = file.getName();
            final Matcher matcher = pattern.matcher(fileName);
            if (matcher.matches()) {
                final int fileNumber = Integer.parseInt(matcher.group(1));
                fileNumbers.add(fileNumber);
            }
        }

        final int nextFileNumber = fileNumbers.stream()
                .max(Integer::compareTo)
                .map(num -> num + 1)
                .orElse(1);

        return "blob" + nextFileNumber + "." + fileExtension;
    }

}
