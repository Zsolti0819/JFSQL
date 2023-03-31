package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import java.io.File;
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
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class ReaderXmlImpl implements Reader {

    private static final Logger logger = LogManager.getLogger(ReaderXmlImpl.class);

    @Override
    public String getFileExtension() {
        return "xml";
    }

    @Override
    public String getSchemaFileExtension() {
        return "xsd";
    }

    @Override
    public List<Entry> readEntriesFromTable(final Table table) throws SQLException {
        final String tableFile = table.getTableFile();
        final List<Entry> entries = new ArrayList<>();

        if (!Path.of(tableFile).toFile().exists()) {
            logger.debug("The table file '{}' doesn't exist, returning an empty list.", tableFile);
            return entries;
        }

        try {
            final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            final Document document = documentBuilder.parse(tableFile);
            document.getDocumentElement().normalize();
            final NodeList entryList = document.getElementsByTagName("Entry");

            final Map<String, String> columnsAndTypes = table.getColumnsAndTypes();
            for (int i = 0; i < entryList.getLength(); i++) {
                final Element entry = (Element) entryList.item(i);
                final LinkedHashMap<String, String> columnsAndValues = new LinkedHashMap<>();
                for (final String column : columnsAndTypes.keySet()) {
                    final String value = getValue(table, column, entry);
                    columnsAndValues.put(column, value);
                }
                entries.add(new Entry(columnsAndValues));
            }
        } catch (final ParserConfigurationException | SAXException | IOException e) {
            throw new SQLException(e);
        }
        return entries;
    }

    private String getValue(final Table table, final String column, final Element entry) throws SQLException {
        if (entry.getElementsByTagName(column).item(0) == null) {
            return null;
        }
        if (Objects.equals(table.getColumnsAndTypes().get(column), "BLOB")) {
            return readBlob(entry.getElementsByTagName(column).item(0).getTextContent());
        } else {
            return entry.getElementsByTagName(column).item(0).getTextContent();
        }
    }

    @Override
    public Table readSchema(final String pathToSchema) throws SQLException {
        final Map<String, String> columnsAndTypes = new LinkedHashMap<>();
        final Map<String, Boolean> notNullColumns = new LinkedHashMap<>();
        try {
            final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            final Document document = documentBuilder.parse(pathToSchema);

            final NodeList nodeList1 = document.getElementsByTagName("xs:element");
            final NodeList nodeList2 = ((Element) nodeList1.item(0)).getElementsByTagName("xs:complexType");
            final NodeList nodeList3 = ((Element) nodeList2.item(0)).getElementsByTagName("xs:sequence");
            final NodeList nodeList4 = ((Element) nodeList3.item(0)).getElementsByTagName("xs:element");
            final NodeList nodeList5 = ((Element) nodeList4.item(0)).getElementsByTagName("xs:complexType");
            final NodeList nodeList6 = ((Element) nodeList5.item(0)).getElementsByTagName("xs:sequence");
            final NodeList nodeList7 = ((Element) nodeList6.item(0)).getElementsByTagName("xs:element");

            final int variableLengths = nodeList7.getLength();
            final String[] columnNames = new String[variableLengths];
            final String[] columnTypes = new String[variableLengths];

            for (int i = 0; i < variableLengths; i++) {
                final Node node2 = nodeList7.item(i);
                final Element element = (Element) node2;
                columnNames[i] = element.getAttribute("name");
                columnTypes[i] = DatatypeConverter.convertFromXsToSql(element.getAttribute("type"));
                columnsAndTypes.put(columnNames[i], columnTypes[i]);
                if (StringUtils.EMPTY.equals(element.getAttribute("minOccurs"))) {
                    notNullColumns.put(columnNames[i], true);
                } else if ("0".equals(element.getAttribute("minOccurs"))) {
                    notNullColumns.put(columnNames[i], false);
                }
            }
            return Table.builder()
                .schemaFile(pathToSchema)
                .columnsAndTypes(columnsAndTypes)
                .notNullColumns(notNullColumns)
                .build();
        } catch (final ParserConfigurationException | SAXException | IOException e) {
            throw new SQLException(e);
        }
    }

    @Override
    public List<Table> readTablesFromDatabaseFile(final Database database) throws SQLException {
        final List<Table> tables = new ArrayList<>();
        try {
            final String url = String.valueOf(database.getUrl());
            final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            final Document document = documentBuilder.parse(url);
            final XPath xpath = XPathFactory.newInstance().newXPath();
            final NodeList nodeList = (NodeList) xpath.evaluate("//Table", document, XPathConstants.NODESET);
            final int tableLengths = nodeList.getLength();
            final String[] tableNames = new String[tableLengths];

            for (int i = 0; i < tableLengths; i++) {
                tableNames[i] = (String) xpath.evaluate("@name", nodeList.item(i), XPathConstants.STRING);
                final String tableName = tableNames[i];
                final String xmlPath = (String) xpath.evaluate("pathToTable/text()", nodeList.item(i),
                    XPathConstants.STRING);
                final String xsdPath = (String) xpath.evaluate("pathToSchema/text()", nodeList.item(i),
                    XPathConstants.STRING);
                final Table schema = readSchema(xsdPath);
                final Table table = Table.builder()
                    .name(tableName)
                    .tableFile(xmlPath)
                    .schemaFile(schema.getSchemaFile())
                    .columnsAndTypes(schema.getColumnsAndTypes())
                    .notNullColumns(schema.getNotNullColumns())
                    .build();
                tables.add(table);
            }
        } catch (final ParserConfigurationException | SAXException | XPathExpressionException | IOException e) {
            throw new SQLException(e);
        }
        return tables;
    }

    @Override
    public String readBlob(final String pathToBlob) throws SQLException {
        try {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder builder = factory.newDocumentBuilder();
            final Document document = builder.parse(pathToBlob);
            return document.getElementsByTagName("blob").item(0).getTextContent();

        } catch (final ParserConfigurationException | SAXException | IOException e) {
            throw new SQLException(e);
        }
    }

    @Override
    public Set<File> getFilesInDatabaseFile(final Database database) throws SQLException {
        final String xmlFilePath = String.valueOf(database.getUrl());
        try {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder builder = factory.newDocumentBuilder();
            final Document doc = builder.parse(xmlFilePath);

            final Set<File> fileSet = new HashSet<>();

            NodeList pathList = doc.getElementsByTagName("pathToTable");
            for (int i = 0; i < pathList.getLength(); i++) {
                final String path = pathList.item(i).getTextContent();
                fileSet.add(new File(path));
            }

            pathList = doc.getElementsByTagName("pathToSchema");
            for (int i = 0; i < pathList.getLength(); i++) {
                final String path = pathList.item(i).getTextContent();
                fileSet.add(new File(path));
            }

            return fileSet;
        } catch (final ParserConfigurationException | SAXException | IOException e) {
            throw new SQLException(e);
        }
    }

}
