package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Schema;
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
import java.sql.SQLException;
import java.util.ArrayList;
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
    public List<Entry> readEntriesFromTable(final Table table) throws SQLException {
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

            final Map<String, String> columnsAndTypes = table.getSchema().getColumnsAndTypes();
            for (int i = 0; i < entryList.size(); i++) {
                final JsonObject entryObject = entryList.get(i).getAsJsonObject();
                final LinkedHashMap<String, String> columnsAndValues = new LinkedHashMap<>();
                for (final String column : columnsAndTypes.keySet()) {
                    final String value = getValue(table, column, entryObject);
                    columnsAndValues.put(column, value);
                }
                entries.add(new Entry(columnsAndValues));
            }
        } catch (IOException e) {
            throw new SQLException(e);
        }
        return entries;
    }

    private String getValue(final Table table, final String column, final JsonObject entryObject) throws SQLException {
        if (entryObject.get(column).isJsonNull()) {
            return null;
        } else {
            if (Objects.equals(table.getSchema().getColumnsAndTypes().get(column), "BLOB")) {
                return readBlob(entryObject.get(column).getAsString());
            } else {
                return entryObject.get(column).getAsString();
            }
        }
    }

    @Override
    public Schema readSchema(final String pathToSchema) throws SQLException {
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
            return new Schema(pathToSchema, columnsAndTypes, notNullColumns);
        } catch (IOException e) {
            throw new SQLException(e);
        }
    }

    @Override
    public List<Table> readTablesFromDatabaseFile(final Database database) throws SQLException {
        final List<Table> tables = new ArrayList<>();
        final String url = String.valueOf(database.getUrl());
        try (final FileReader fileReader = new FileReader(url)) {
            final JsonElement json = JsonParser.parseReader(fileReader);
            final JsonObject jsonObject = json.getAsJsonObject();
            final JsonArray nodeList = jsonObject.getAsJsonArray("Table");
            final int tableLengths = nodeList.size();
            for (int i = 0; i < tableLengths; i++) {
                final JsonObject jsonTableObject = nodeList.get(i).getAsJsonObject();
                final String tableName = jsonTableObject.get("name").getAsString();
                final String tablePath = jsonTableObject.get("pathToTable").getAsString();
                final String schemaPath = jsonTableObject.get("pathToSchema").getAsString();
                final Schema schema = readSchema(schemaPath);
                final Table table = new Table(tableName, tablePath, schema, new ArrayList<>());
                tables.add(table);
            }
        } catch (IOException e) {
            throw new SQLException(e);
        }
        return tables;
    }

    @Override
    public String readBlob(final String pathToBlob) throws SQLException {
        try (final FileReader fileReader = new FileReader(pathToBlob)) {
            final JsonElement json = JsonParser.parseReader(fileReader);
            final JsonObject jsonObject = json.getAsJsonObject();
            return jsonObject.get("blob").getAsString();
        } catch (IOException e) {
            throw new SQLException(e);
        }
    }

    @Override
    public Set<File> getFilesInDatabaseFile(final Database database) throws SQLException {
        final String jsonFilePath = String.valueOf(database.getUrl());
        try (FileReader fileReader = new FileReader(jsonFilePath)) {
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
        } catch (IOException e) {
            throw new SQLException(e);
        }

    }

}
