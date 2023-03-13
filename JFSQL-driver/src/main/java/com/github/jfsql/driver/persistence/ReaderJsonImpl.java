package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.*;

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
    public List<Entry> readTable(final Table table) throws SQLException {
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

            final String[] columns = table.getColumns();
            final String[] values = new String[columns.length];

            for (int i = 0; i < entryList.size(); i++) {
                final JsonObject entry = entryList.get(i).getAsJsonObject();
                for (int j = 0; j < columns.length; j++) {
                    if (Objects.equals(table.getTypes()[j], "BLOB")) {
                        values[j] = readBlob(entry.get(columns[j]).getAsString());
                    } else {
                        values[j] = entry.get(columns[j]).getAsString();
                    }
                }

                final LinkedHashMap<String, String> columnsAndValues = new LinkedHashMap<>();
                for (int j = 0; j < columns.length; j++) {
                    columnsAndValues.put(columns[j], values[j]);
                }

                entries.add(new Entry(columnsAndValues));
            }
        } catch (final IOException e) {
            throw new SQLException("Failed to read the table.\n" + e.getMessage());
        }
        return entries;
    }

    @Override
    public Table readSchema(final String pathToSchema) throws SQLException {
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
                if (Objects.equals(columnType, "TEXT") && formatElement != null && Objects.equals(formatElement.getAsString(), "iri-reference")) {
                    columnType = "BLOB";
                }
                columnsAndTypes.put(columnName, columnType);
                if (typeJsonArray.size() == 2 && Objects.equals(typeJsonArray.get(1).getAsString(), "null")) {
                    notNullColumns.put(columnName, false);
                } else if (typeJsonArray.size() == 1) {
                    notNullColumns.put(columnName, true);
                }
            }
            return new Table(null, null, null, columnsAndTypes, notNullColumns);
        } catch (final IOException e) {
            throw new SQLException("Failed to read the schema for the table.\n" + e.getMessage());
        }
    }

    @Override
    public List<Table> readDatabaseFile(final Database database) throws SQLException {
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
                final Table schema = readSchema(schemaPath);
                if (database.getTables() == null) {
                    database.setTables(new ArrayList<>());
                }
                final Table table = new Table(tableName, tablePath, schemaPath, schema.getColumnsAndTypes(), schema.getNotNullColumns());
                tables.add(table);
            }
        } catch (final IOException | SQLException e) {
            throw new SQLException("Failed to read the database file.\n" + e.getMessage());
        }
        return tables;
    }

    @Override
    public String readBlob(final String pathToBlob) throws SQLException {
        try (final FileReader fileReader = new FileReader(pathToBlob)) {
            final JsonElement json = JsonParser.parseReader(fileReader);
            final JsonObject jsonObject = json.getAsJsonObject();
            return jsonObject.get("blob").getAsString();
        } catch (final IOException e) {
            throw new SQLException("Failed to read blob.\n" + e.getMessage());
        }
    }

    @Override
    public boolean pathIsPresentInDatabaseFile(final Database database, final String pathToCheck) throws SQLException {
        final String jsonFilePath = String.valueOf(database.getUrl());
        try {
            final FileReader fileReader = new FileReader(jsonFilePath);
            final JsonElement jsonElement = JsonParser.parseReader(fileReader);
            if (jsonElement.isJsonObject()) {
                final JsonObject rootObject = jsonElement.getAsJsonObject();
                final JsonArray tableArray = rootObject.getAsJsonArray("Table");
                for (final JsonElement tableElement : tableArray) {
                    final JsonObject tableObject = tableElement.getAsJsonObject();
                    final String pathToTable = tableObject.get("pathToTable").getAsString();
                    final String pathToSchema = tableObject.get("pathToSchema").getAsString();
                    if (pathToTable.equals(pathToCheck) || pathToSchema.equals(pathToCheck)) {
                        fileReader.close();
                        return true;
                    }
                }
            }
            fileReader.close();
            return false;
        } catch (final IOException e) {
            throw new SQLException("Failed to verify the presence of the files in the folder.\n" + e.getMessage());
        }
    }

}
