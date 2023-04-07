package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ReaderJsonImpl implements Reader {

    private static final Logger logger = LogManager.getLogger(ReaderJsonImpl.class);

    @Override
    public String getFileExtension() {
        return "json";
    }

    @Override
    public String getSchemaFileExtension() {
        return getFileExtension();
    }

    @Override
    public List<Entry> readEntriesFromTable(final Table table) throws IOException {
        final String tableFile = table.getTableFile();
        final List<Entry> entries = new ArrayList<>();

        if (!Path.of(tableFile).toFile().exists()) {
            logger.debug("The table file '{}' doesn't exist, returning an empty list.", tableFile);
            return entries;
        }

        try (final FileReader fileReader = new FileReader(tableFile)) {
            final JsonElement json = JsonParser.parseReader(fileReader);
            final JsonObject jsonObject = json.getAsJsonObject();
            final JsonArray entryList = jsonObject.getAsJsonArray("Entry");

            final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
            for (int i = 0; i < entryList.size(); i++) {
                final JsonObject entryObject = entryList.get(i).getAsJsonObject();
                final LinkedHashMap<String, String> columnsAndValues = new LinkedHashMap<>();
                for (final String column : columnsAndTypes.keySet()) {
                    final String value = getValue(column, entryObject);
                    columnsAndValues.put(column, value);
                }
                entries.add(new Entry(columnsAndValues, new HashMap<>()));
            }
        }
        return entries;
    }

    private String getValue(final String column, final JsonObject entryObject) {
        if (entryObject.get(column).isJsonNull()) {
            return null;
        }
        return entryObject.get(column).getAsString();
    }

    @Override
    public Table readSchema(final String pathToSchema) throws IOException {
        final Map<String, String> columnsAndTypes = new LinkedHashMap<>();
        final Map<String, Boolean> notNullColumns = new LinkedHashMap<>();
        try (final FileReader fileReader = new FileReader(pathToSchema)) {
            final JsonElement root = JsonParser.parseReader(fileReader);
            final JsonObject firstPropertiesObject = root.getAsJsonObject().get("properties").getAsJsonObject();

            final JsonObject tableObject = firstPropertiesObject.get("Entry").getAsJsonObject();
            final JsonObject itemsObject = tableObject.get("items").getAsJsonObject();

            final JsonObject propertiesObject = itemsObject.get("properties").getAsJsonObject();

            for (final Map.Entry<String, JsonElement> entry : propertiesObject.entrySet()) {
                final String columnName = entry.getKey();
                final JsonArray typeJsonArray = entry.getValue().getAsJsonObject().get("type").getAsJsonArray();
                String columnType = DatatypeConverter.convertFromJsonToSql(typeJsonArray.get(0).getAsString());
                final JsonElement formatElement = entry.getValue().getAsJsonObject().get("format");
                if (Objects.equals(columnType, "TEXT") && formatElement != null && Objects.equals(
                    formatElement.getAsString(), "iri-reference")) {
                    columnType = "BLOB";
                }
                columnsAndTypes.put(columnName, columnType);
                if (typeJsonArray.size() == 2 && Objects.equals(typeJsonArray.get(1).getAsString(), "null")) {
                    notNullColumns.put(columnName, false);
                } else if (typeJsonArray.size() == 1) {
                    notNullColumns.put(columnName, true);
                }
            }
            return Table.builder()
                .schemaFile(pathToSchema)
                .columnsAndTypes(columnsAndTypes)
                .notNullColumns(notNullColumns)
                .build();
        }
    }

    @Override
    public List<Table> readTablesFromDatabaseFile(final Database database) throws IOException {
        final List<Table> tables = new ArrayList<>();
        final String URL = String.valueOf(database.getURL());
        try (final FileReader fileReader = new FileReader(URL)) {
            final JsonElement json = JsonParser.parseReader(fileReader);
            final JsonObject jsonObject = json.getAsJsonObject();
            final JsonArray nodeList = jsonObject.getAsJsonArray("Table");
            final int tableLengths = nodeList.size();
            for (int i = 0; i < tableLengths; i++) {
                final JsonObject jsonTableObject = nodeList.get(i).getAsJsonObject();
                final String tableName = jsonTableObject.get("name").getAsString();
                final String tablePath = jsonTableObject.get("pathToTable").getAsString();
                final String schemaPath = jsonTableObject.get("pathToSchema").getAsString();
                final Table schema = readSchema(schemaPath);
                final Table table = Table.builder()
                    .name(tableName)
                    .tableFile(tablePath)
                    .schemaFile(schema.getSchemaFile())
                    .columnsAndTypes(schema.getColumnsAndTypes())
                    .notNullColumns(schema.getNotNullColumns())
                    .build();
                tables.add(table);
            }
        }
        return tables;
    }

    @Override
    public String readBlob(final String pathToBlob) throws IOException {
        try (final FileReader fileReader = new FileReader(pathToBlob)) {
            final JsonElement json = JsonParser.parseReader(fileReader);
            final JsonObject jsonObject = json.getAsJsonObject();
            return jsonObject.get("blob").getAsString();
        }
    }

    @Override
    public Set<File> getFilesFromDatabaseFile(final Database database) throws IOException {
        final String jsonFilePath = String.valueOf(database.getURL());
        try (final FileReader fileReader = new FileReader(jsonFilePath)) {
            final JsonElement jsonElement = JsonParser.parseReader(fileReader);
            final Set<File> files = new HashSet<>();
            if (jsonElement.isJsonObject()) {
                final JsonObject rootObject = jsonElement.getAsJsonObject();
                final JsonArray tableArray = rootObject.getAsJsonArray("Table");
                for (final JsonElement tableElement : tableArray) {
                    final JsonObject tableObject = tableElement.getAsJsonObject();
                    final String pathToTable = tableObject.get("pathToTable").getAsString();
                    final String pathToSchema = tableObject.get("pathToSchema").getAsString();
                    files.add(new File(pathToTable));
                    files.add(new File(pathToSchema));
                }
            }
            return files;
        }
    }

    @Override
    public Set<File> getBlobsFromTables(final Database database) throws IOException {
        final Set<File> fileSet = new HashSet<>();
        for (final Table table : database.getTables()) {
            for (final Map.Entry<String, String> entry : table.getColumnsAndTypes().entrySet()) {
                if (entry.getValue().equals("BLOB")) {
                    try (final FileReader fileReader = new FileReader(table.getTableFile())) {
                        final JsonObject rootObject = JsonParser.parseReader(fileReader).getAsJsonObject();
                        final JsonArray tableArray = rootObject.getAsJsonArray("Entry");
                        tableArray.forEach(tableElement -> {
                            final JsonElement value = tableElement.getAsJsonObject().get(entry.getKey());
                            if (!value.isJsonNull()) {
                                final String path = value.getAsString();
                                fileSet.add(new File(path));
                            }
                        });
                    }
                }
            }
        }
        return fileSet;
    }

}
