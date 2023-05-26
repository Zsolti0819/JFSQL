package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.SchemaValidationException;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.github.jfsql.driver.validation.JsonSchemaValidator;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.stream.JsonWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class WriterJsonImpl extends Writer {

    private static final Logger logger = LogManager.getLogger(WriterJsonImpl.class);
    private static final JsonSchemaValidator JSON_SCHEMA_VALIDATOR = JsonSchemaValidator.INSTANCE;
    private final Gson gson;

    public WriterJsonImpl(final boolean useSchemaValidation) {
        super(useSchemaValidation);
        gson = new GsonBuilder().serializeNulls().create();
    }

    private byte[] beautify(final JsonObject jsonObject) {
        final StringWriter stringWriter = new StringWriter();
        final JsonWriter jsonWriter = new JsonWriter(stringWriter);
        jsonWriter.setIndent("  ");
        gson.toJson(jsonObject, JsonObject.class, jsonWriter);
        return stringWriter.toString().getBytes();
    }

    @Override
    public void writeTable(final Table table) throws IOException, SchemaValidationException {
        final String tableFile = table.getTableFile();
        final FileOutputStream fileOutputStream = new FileOutputStream(tableFile);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();
        final List<Entry> entries = table.getEntries();
        final JsonObject root = new JsonObject();
        final JsonArray entryArray = new JsonArray();
        for (final Entry entry : entries) {
            final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
            final JsonObject entryObject = new JsonObject();
            for (final String column : columnsAndValues.keySet()) {
                checkTypeAndValueThenAddProperty(table, entry, column, entryObject);
            }
            entryArray.add(entryObject);
        }
        root.add("Entry", entryArray);
        final byte[] jsonBytes = beautify(root);
        fileOutputStream.write(jsonBytes);
        fileLock.release();
        fileOutputStream.close();

        if (useSchemaValidation) {
            final String schemaFile = table.getSchemaFile();
            final boolean isValid = JSON_SCHEMA_VALIDATOR.schemaIsValid(schemaFile, tableFile);
            if (!isValid) {
                throw new SchemaValidationException("'" + tableFile + "' is not valid against '" + schemaFile + "'");
            }
        }
    }

    private void checkTypeAndValueThenAddProperty(final Table table, final Entry entry, final String column,
        final JsonObject entryObject) throws IOException {
        final String value = entry.getColumnsAndValues().get(column);
        final String type = table.getColumnsAndTypes().get(column);
        if (value == null || "null".equals(value)) {
            entryObject.add(column, null);
        } else {
            switch (type) {
                case "INTEGER":
                    entryObject.addProperty(column, Integer.parseInt(value));
                    break;
                case "REAL":
                    entryObject.addProperty(column, Double.parseDouble(value));
                    break;
                case "TEXT":
                    entryObject.addProperty(column, value);
                    break;
                case "BLOB":
                    entryObject.addProperty(column, writeBlob(table, entry, column));
                    break;
                default:
                    throw new IllegalStateException("Unsupported data type '" + type);
            }
        }
    }

    @Override
    public void writeSchema(final Table table) throws IOException {
        final String schemaFile = table.getSchemaFile();
        final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

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
        for (final String columnName : table.getNotNullColumns().keySet()) {
            if (Boolean.TRUE.equals(table.getNotNullColumns().get(columnName))) {
                requiredColumns.add(columnName);
            }
        }

        final JsonObject thirdProperties = new JsonObject();
        items.add("properties", thirdProperties);
        for (final String column : table.getColumnsAndTypes().keySet()) {
            final JsonObject columnName = new JsonObject();
            thirdProperties.add(column, columnName);
            final JsonArray columns = new JsonArray();
            final String jsonDatatype = DatatypeConverter.convertFromSqlToJson(table.getColumnsAndTypes().get(column));
            if (Boolean.TRUE.equals(table.getNotNullColumns().get(column))) {
                columns.add(jsonDatatype);
            } else if (Boolean.FALSE.equals(table.getNotNullColumns().get(column))) {
                columns.add(jsonDatatype);
                columns.add("null");
            }
            columnName.add("type", columns);
            if ("BLOB".equals(table.getColumnsAndTypes().get(column))) {
                columnName.addProperty("format", "iri-reference");
            }
        }
        final byte[] jsonBytes = beautify(root);
        fileOutputStream.write(jsonBytes);

        fileLock.release();
        fileOutputStream.close();

    }

    @Override
    public void writeDatabaseFile(final Database database) throws IOException {
        final String databaseURL = database.getURL();
        final FileOutputStream fileOutputStream = new FileOutputStream(databaseURL);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

        final JsonObject root = new JsonObject();
        root.addProperty("Database", String.valueOf(Path.of(database.getURL()).getParent().getFileName()));
        final List<Table> tables = database.getTables();

        final Gson gson = new Gson();
        final JsonObject[] tablesArray = new JsonObject[tables.size()];
        IntStream.range(0, tables.size()).forEachOrdered(i -> {
            final JsonObject tableJsonObject = new JsonObject();
            tableJsonObject.addProperty("name", tables.get(i).getName());
            tableJsonObject.addProperty("pathToTable", tables.get(i).getTableFile());
            tableJsonObject.addProperty("pathToSchema", tables.get(i).getSchemaFile());
            tablesArray[i] = tableJsonObject;
        });
        root.add("Table", gson.toJsonTree(tablesArray));
        final byte[] jsonBytes = beautify(root);
        fileOutputStream.write(jsonBytes);

        fileLock.release();
        fileOutputStream.close();

    }

    @Override
    public String writeBlob(final Table table, final Entry entry, final String column) throws IOException {
        final Map<String, LargeObject> columnsAndBlobs = entry.getColumnsAndBlobs();
        final LargeObject largeObject = columnsAndBlobs.get(column);
        final String blobURL = largeObject.getURL();
        logger.trace("blob URL = {}", blobURL);
        final String blobValue = largeObject.getValue();
        logger.trace("blob value = {}", blobValue);
        final Path tableParent = Path.of(table.getTableFile()).getParent();
        final Path blobParent = Path.of(String.valueOf(tableParent), "blob");
        Files.createDirectories(blobParent);
        final FileOutputStream fileOutputStream = new FileOutputStream(blobURL);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

        final JsonObject root = new JsonObject();
        root.addProperty("blob", blobValue);
        final byte[] jsonBytes = beautify(root);
        fileOutputStream.write(jsonBytes);
        fileLock.release();
        fileOutputStream.close();

        return blobURL;
    }

}

