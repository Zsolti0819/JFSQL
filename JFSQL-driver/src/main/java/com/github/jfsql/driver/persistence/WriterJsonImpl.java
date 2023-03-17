package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.github.jfsql.driver.validation.JsonSchemaValidator;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.stream.JsonWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class WriterJsonImpl extends Writer {

    private static final JsonSchemaValidator JSON_SCHEMA_VALIDATOR = JsonSchemaValidator.INSTANCE;

    private String beautify(final Object object) {
        final Gson gson = new GsonBuilder().serializeNulls().setPrettyPrinting().create();
        final StringWriter stringWriter = new StringWriter();
        final JsonWriter jsonWriter = new JsonWriter(stringWriter);
        jsonWriter.setIndent("  ");
        gson.toJson(object, object.getClass(), jsonWriter);
        return stringWriter.toString();
    }

    @Override
    public void writeTable(final Table table) throws SQLException {
        final String tableFile = table.getTableFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(tableFile);
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final List<Entry> entries = table.getEntries();
            final JsonObject root = new JsonObject();

            final JsonArray entryArray = new JsonArray();
            for (final Entry entry : entries) {
                final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
                final JsonObject entryObject = new JsonObject();
                for (int i = 0; i < columnsAndValues.size(); i++) {
                    checkTypeAndValueThenAddProperty(table, entry, entryObject, i);
                }
                entryArray.add(entryObject);
            }
            root.add("Entry", entryArray);

            final String jsonString = beautify(root);
            fileOutputStream.write(jsonString.getBytes());
        } catch (final IOException e) {
            throw new SQLException("Failed to write the table." + e.getMessage());
        }

        if (USE_SCHEMA_VALIDATION.equals(true)) {
            final String schemaFile = table.getSchemaFile();
            final boolean isValid = JSON_SCHEMA_VALIDATOR.schemaIsValid(schemaFile, tableFile);
            if (!isValid) {
                throw new SQLException("\"" + tableFile + "\" is not valid against \"" + schemaFile + "\"");
            }
        }
    }

    private void checkTypeAndValueThenAddProperty(final Table table, final Entry entry, final JsonObject entryObject,
        final int index) throws SQLException {
        if (entry.getValues()[index] == null) {
            entryObject.add(entry.getColumns()[index], null);
        } else if (entry.getValues()[index] != null) {
            if (Objects.equals("INTEGER", table.getTypes()[index])) {
                entryObject.addProperty(entry.getColumns()[index], Integer.parseInt(entry.getValues()[index]));
            } else if (Objects.equals("REAL", table.getTypes()[index])) {
                entryObject.addProperty(entry.getColumns()[index], Double.parseDouble(entry.getValues()[index]));
            } else if (Objects.equals("TEXT", table.getTypes()[index])) {
                entryObject.addProperty(entry.getColumns()[index], entry.getValues()[index]);
            } else if (Objects.equals("BLOB", table.getTypes()[index])) {
                final Path blobPath = writeBlob(table, entry.getValues()[index]);
                if (Objects.equals(String.valueOf(blobPath), "null")) {
                    entryObject.add(entry.getColumns()[index], null);
                } else {
                    entryObject.addProperty(entry.getColumns()[index], String.valueOf(blobPath));
                }
            }
        }
    }

    @Override
    public void writeSchema(final Table table) throws SQLException {
        final String schemaFile = table.getSchemaFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile);
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final String[] columnNames = table.getColumns();
            final String[] columnTypes = table.getTypes();

            final JsonObject root = new JsonObject();
            root.addProperty("$schema", "http://json-schema.org/draft-06/schema#");
            root.addProperty("type", "object");

            final JsonArray requiredEntry = new JsonArray();
            requiredEntry.add("Entry");
            root.add("required", requiredEntry);

            final JsonObject secondProperties = new JsonObject();
            root.add("properties", secondProperties);
            final JsonObject entry = new JsonObject();
            secondProperties.add("Entry", entry);

            entry.addProperty("type", "array");
            final JsonObject items = new JsonObject();
            entry.add("items", items);

            items.addProperty("type", "object");
            final JsonArray requiredColumns = new JsonArray();
            items.add("required", requiredColumns);
            for (final String columnName : columnNames) {
                if (Boolean.TRUE.equals(table.getNotNullColumns().get(columnName))) {
                    requiredColumns.add(columnName);
                }
            }

            final JsonObject thirdProperties = new JsonObject();
            items.add("properties", thirdProperties);
            for (int i = 0; i < columnTypes.length; i++) {
                final JsonObject columnName = new JsonObject();
                thirdProperties.add(columnNames[i], columnName);
                final JsonArray columns = new JsonArray();
                final String jsonDatatype = DatatypeConverter.convertFromSqlToJson(columnTypes[i]);
                if (Boolean.TRUE.equals(table.getNotNullColumns().get(columnNames[i]))) {
                    columns.add(jsonDatatype);
                } else if (Boolean.FALSE.equals(table.getNotNullColumns().get(columnNames[i]))) {
                    columns.add(jsonDatatype);
                    columns.add("null");
                }
                columnName.add("type", columns);
                if (Objects.equals(columnTypes[i], "BLOB")) {
                    columnName.addProperty("format", "iri-reference");
                }
            }

            final String jsonString = beautify(root);
            fileOutputStream.write(jsonString.getBytes());
        } catch (final IOException e) {
            throw new SQLException("Failed to write the schema.\n" + e.getMessage());
        }
    }

    @Override
    public void writeDatabaseFile(final Database database) throws SQLException {
        final Path databaseFilePath = database.getUrl();
        final String databaseFileParentPath = String.valueOf(databaseFilePath.getParent());
        final Path databaseFolderName = Path.of(databaseFileParentPath);
        try (final FileOutputStream fileOutputStream = new FileOutputStream(String.valueOf(databaseFilePath));
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final JsonObject root = new JsonObject();
            root.addProperty("Database", String.valueOf(databaseFolderName.getFileName()));
            final List<Table> tables = database.getTables();

            final Gson gson = new Gson();
            final JsonObject[] tablesArray = new JsonObject[tables.size()];
            for (int i = 0; i < tables.size(); i++) {
                final JsonObject tableJsonObject = new JsonObject();
                tableJsonObject.addProperty("name", tables.get(i).getName());
                tableJsonObject.addProperty("pathToTable", tables.get(i).getTableFile());
                tableJsonObject.addProperty("pathToSchema", tables.get(i).getSchemaFile());
                tablesArray[i] = tableJsonObject;
            }
            root.add("Table", gson.toJsonTree(tablesArray));

            final String jsonString = beautify(root);
            fileOutputStream.write(jsonString.getBytes());
        } catch (final IOException e) {
            throw new SQLException("Failed to write the database file." + e.getMessage());
        }
    }

    @Override
    Path writeBlob(final Table table, final String value) throws SQLException {
        if (Objects.equals(value, "null")) {
            return null;
        }
        final Path tableParent = Path.of(table.getTableFile()).getParent();
        final Path blobParent = Path.of(tableParent + File.separator + "blob");
        try {
            Files.createDirectories(blobParent);
        } catch (final IOException e) {
            throw new SQLException("Failed to create directory for BLOBs.\n" + e.getMessage());
        }
        final String newBlobName = incrementFileName(blobParent, "json");
        final Path blobPath = Path.of(blobParent + File.separator + newBlobName);
        try (final FileOutputStream fileOutputStream = new FileOutputStream(blobPath.toFile());
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final JsonObject root = new JsonObject();
            root.addProperty("blob", value);
            final String jsonString = beautify(root);
            fileOutputStream.write(jsonString.getBytes());

        } catch (final IOException e) {
            throw new SQLException("Failed to write the blob\n" + e.getMessage());
        }
        return blobPath;
    }

}

