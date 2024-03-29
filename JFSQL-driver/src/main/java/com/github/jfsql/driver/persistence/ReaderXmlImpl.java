package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
    public List<Entry> readEntriesFromTable(final Table table) throws IOException {
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
                final Element element = (Element) entryList.item(i);
                final LinkedHashMap<String, String> columnsAndValues = new LinkedHashMap<>();
                for (final String column : columnsAndTypes.keySet()) {
                    final String value = getValue(column, element);
                    columnsAndValues.put(column, value);
                }
                entries.add(new Entry(columnsAndValues, new HashMap<>()));
            }
        } catch (final ParserConfigurationException | SAXException e) {
            throw new IOException(e);
        }
        return entries;
    }

    private String getValue(final String column, final Element entry) {
        final Node node = entry.getElementsByTagName(column).item(0);
        if (node == null) {
            return null;
        }
        return node.getTextContent();
    }

    @Override
    public void setTableMetaDataFromSchema(final Table table) throws IOException {
        final Map<String, String> columnsAndTypes = new LinkedHashMap<>();
        final Map<String, Boolean> notNullColumns = new LinkedHashMap<>();
        try {
            final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            final String pathToSchema = table.getSchemaFile();
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
            table.setColumnsAndTypes(columnsAndTypes);
            table.setNotNullColumns(notNullColumns);
        } catch (final ParserConfigurationException | SAXException e) {
            throw new IOException(e);
        }
    }

    @Override
    public List<Table> readTablesFromDatabaseFile(final Database database) throws IOException {
        final List<Table> tables = new ArrayList<>();
        try {
            final String URL = String.valueOf(database.getURL());
            final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            final Document document = documentBuilder.parse(URL);
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

                final Table table = Table.builder()
                    .name(tableName)
                    .tableFile(xmlPath)
                    .schemaFile(xsdPath)
                    .build();
                setTableMetaDataFromSchema(table);
                tables.add(table);
            }
        } catch (final ParserConfigurationException | SAXException | XPathExpressionException e) {
            throw new IOException(e);
        }
        return tables;
    }

    @Override
    public String readBlob(final String pathToBlob) throws IOException {
        try {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            final DocumentBuilder builder = factory.newDocumentBuilder();
            final Document document = builder.parse(pathToBlob);
            return document.getElementsByTagName("blob").item(0).getTextContent();

        } catch (final ParserConfigurationException | SAXException e) {
            throw new IOException(e);
        }
    }

    @Override
    public Set<File> getBlobsFromTables(final Database database) throws IOException {
        final Set<File> fileSet = new HashSet<>();
        for (final Table table : database.getTables()) {
            for (final Map.Entry<String, String> entry : table.getColumnsAndTypes().entrySet()) {
                if (entry.getValue().equals("BLOB")) {
                    try {
                        final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                        factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
                        final DocumentBuilder builder = factory.newDocumentBuilder();
                        final Document doc = builder.parse(table.getTableFile());

                        final NodeList pathList = doc.getElementsByTagName(entry.getKey());
                        for (int i = 0; i < pathList.getLength(); i++) {
                            final String path = pathList.item(i).getTextContent();
                            fileSet.add(new File(path));
                        }
                    } catch (final ParserConfigurationException | SAXException e) {
                        throw new IOException(e);
                    }
                }
            }
        }
        return fileSet;
    }
}
