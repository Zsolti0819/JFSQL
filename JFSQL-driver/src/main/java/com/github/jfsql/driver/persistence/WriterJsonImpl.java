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
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class WriterJsonImpl extends Writer {

    private static final Logger logger = LogManager.getLogger(WriterJsonImpl.class);
    private static final JsonSchemaValidator JSON_SCHEMA_VALIDATOR = JsonSchemaValidator.INSTANCE;

    public WriterJsonImpl(final boolean useSchemaValidation) {
        super(useSchemaValidation);
    }

    private String beautify(final Object object) {
        final Gson gson = new GsonBuilder().serializeNulls().setPrettyPrinting().create();
        final StringWriter stringWriter = new StringWriter();
        final JsonWriter jsonWriter = new JsonWriter(stringWriter);
        jsonWriter.setIndent("  ");
        gson.toJson(object, object.getClass(), jsonWriter);
        return stringWriter.toString();
    }

    @Override
    public void writeTable(final Table table) throws IOException, SchemaValidationException {
        final String tableFile = table.getTableFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(tableFile)) {
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

            final String jsonString = beautify(root);

            try (final BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(fileOutputStream))) {
                bufferedWriter.write(jsonString);
            }
        }

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
        if (value == null || Objects.equals(value, "null")) {
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
                    throw new IllegalStateException("Unsupported data type '" + type + "'.");
            }
        }
    }

    @Override
    public void writeSchema(final Table schema) throws IOException {
        final String schemaFile = schema.getSchemaFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile)) {
            final List<String> columnNames = new ArrayList<>(schema.getColumnsAndTypes().keySet());
            final List<String> columnTypes = new ArrayList<>(schema.getColumnsAndTypes().values());

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
                if (Boolean.TRUE.equals(schema.getNotNullColumns().get(columnName))) {
                    requiredColumns.add(columnName);
                }
            }

            final JsonObject thirdProperties = new JsonObject();
            items.add("properties", thirdProperties);
            for (int i = 0; i < columnTypes.size(); i++) {
                final JsonObject columnName = new JsonObject();
                thirdProperties.add(columnNames.get(i), columnName);
                final JsonArray columns = new JsonArray();
                final String jsonDatatype = DatatypeConverter.convertFromSqlToJson(columnTypes.get(i));
                if (Boolean.TRUE.equals(schema.getNotNullColumns().get(columnNames.get(i)))) {
                    columns.add(jsonDatatype);
                } else if (Boolean.FALSE.equals(schema.getNotNullColumns().get(columnNames.get(i)))) {
                    columns.add(jsonDatatype);
                    columns.add("null");
                }
                columnName.add("type", columns);
                if (Objects.equals(columnTypes.get(i), "BLOB")) {
                    columnName.addProperty("format", "iri-reference");
                }
            }

            final String jsonString = beautify(root);

            try (final BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(fileOutputStream))) {
                bufferedWriter.write(jsonString);
            }
        }
    }

    @Override
    public void writeDatabaseFile(final Database database) throws IOException {
        final Path databaseFilePath = Path.of(database.getURL());
        try (final FileOutputStream fileOutputStream = new FileOutputStream(databaseFilePath.toFile())) {
            final JsonObject root = new JsonObject();
            root.addProperty("Database", String.valueOf(databaseFilePath.getParent().getFileName()));
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

            try (final BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(fileOutputStream))) {
                bufferedWriter.write(jsonString);
            }
        }
    }

    @Override
    public String writeBlob(final Table table, final Entry entry, final String column) throws IOException {
        final Map<String, LargeObject> columnsAndBlobs = entry.getColumnsAndBlobs();
        final LargeObject largeObject = columnsAndBlobs.get(column);
        final String blobURL = largeObject.getURL();
        logger.trace("blobURL = {}", blobURL);
        final String blobValue = largeObject.getValue();
        logger.trace("blob value = {}", blobValue);
        final Path tableParent = Path.of(table.getTableFile()).getParent();
        final Path blobParent = Path.of(String.valueOf(tableParent), "blob");
        Files.createDirectories(blobParent);

        try (final FileOutputStream fileOutputStream = new FileOutputStream(blobURL)) {
            final JsonObject root = new JsonObject();
            root.addProperty("blob", blobValue);
            final String jsonString = beautify(root);

            try (final BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(fileOutputStream))) {
                bufferedWriter.write(jsonString);
            }
        }
        return blobURL;
    }

}

