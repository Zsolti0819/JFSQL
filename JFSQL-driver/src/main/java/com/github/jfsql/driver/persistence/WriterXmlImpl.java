package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.SchemaValidationException;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.github.jfsql.driver.validation.XmlSchemaValidator;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class WriterXmlImpl extends Writer {

    private static final Logger logger = LogManager.getLogger(WriterXmlImpl.class);
    private static final XmlSchemaValidator XML_SCHEMA_VALIDATOR = XmlSchemaValidator.INSTANCE;

    public WriterXmlImpl(final boolean useSchemaValidation) {
        super(useSchemaValidation);
    }

    private void beautifyAndWrite(final FileOutputStream fileOutputStream, final Document document) throws IOException {
        try {
            final TransformerFactory transformerFactory = TransformerFactory.newInstance();
            transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
            final Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            final Source source = new DOMSource(document);

            try (final BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(fileOutputStream)) {
                final Result result = new StreamResult(bufferedOutputStream);
                transformer.transform(source, result);
            }
        } catch (final TransformerException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void writeTable(final Table table) throws IOException, SchemaValidationException {
        final String tableFile = table.getTableFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(tableFile)) {
            final String tableName = table.getName();
            final List<Entry> entries = table.getEntries();

            final DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Document document = documentBuilder.newDocument();
            final Element root = document.createElement(tableName);

            for (final Entry entry : entries) {
                final Map<String, String> columnsAndValues = entry.getColumnsAndValues();
                final Element entryElement = document.createElement("Entry");
                for (final String column : columnsAndValues.keySet()) {
                    checkTypeAndValueThenAddProperty(table, entry, column, document, entryElement);
                }
                root.appendChild(entryElement);
            }
            document.appendChild(root);
            beautifyAndWrite(fileOutputStream, document);
        } catch (final ParserConfigurationException e) {
            throw new IOException(e);
        }

        if (useSchemaValidation) {
            final String schemaFile = table.getSchemaFile();
            final boolean isValid = XML_SCHEMA_VALIDATOR.schemaIsValid(schemaFile, tableFile);
            if (!isValid) {
                throw new SchemaValidationException("'" + tableFile + "' is not valid against '" + schemaFile + "'");
            }
        }
    }

    private void checkTypeAndValueThenAddProperty(final Table table, final Entry entry, final String column,
        final Document document, final Element element) throws IOException {
        final String value = entry.getColumnsAndValues().get(column);
        final String type = table.getColumnsAndTypes().get(column);
        if (value == null || Objects.equals(value, "null")) {
            return;
        }
        final Element row;
        if (Objects.equals(type, "BLOB")) {
            row = document.createElement(column);
            row.setTextContent(writeBlob(table, entry, column));
        } else {
            row = document.createElement(column);
            row.setTextContent(value);
        }
        element.appendChild(row);
    }

    @Override
    public void writeSchema(final Table schema) throws IOException {
        final String tableName = String.valueOf(Path.of(schema.getSchemaFile()).getFileName()).replace(".xsd", "");
        final String schemaFile = schema.getSchemaFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile)) {
            final List<String> columnNames = new ArrayList<>(schema.getColumnsAndTypes().keySet());
            final List<String> columnTypes = new ArrayList<>(schema.getColumnsAndTypes().values());
            final DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Document document = documentBuilder.newDocument();

            final Element root = document.createElement("xs:schema");
            root.setAttribute("elementFormDefault", "qualified");
            root.setAttribute("xmlns:xs", "http://www.w3.org/2001/XMLSchema");
            document.appendChild(root);

            final Element tableElement = document.createElement("xs:element");
            tableElement.setAttribute("name", tableName);
            root.appendChild(tableElement);

            final Element complexTypeElement = document.createElement("xs:complexType");
            tableElement.appendChild(complexTypeElement);

            final Element sequenceElement = document.createElement("xs:sequence");
            complexTypeElement.appendChild(sequenceElement);

            final Element entryElement = document.createElement("xs:element");
            entryElement.setAttribute("name", "Entry");
            entryElement.setAttribute("maxOccurs", "unbounded");
            entryElement.setAttribute("minOccurs", "0");
            sequenceElement.appendChild(entryElement);

            final Element complexTypeElement2 = document.createElement("xs:complexType");
            entryElement.appendChild(complexTypeElement2);

            final Element sequenceElement2 = document.createElement("xs:sequence");
            complexTypeElement2.appendChild(sequenceElement2);

            for (int i = 0; i < columnTypes.size(); i++) {
                final Element column = document.createElement("xs:element");
                final Attr columnType = document.createAttribute("type");
                columnType.setValue(DatatypeConverter.convertFromSqlToXs(columnTypes.get(i)));
                final Attr columnName = document.createAttribute("name");
                if (Boolean.FALSE.equals(schema.getNotNullColumns().get(columnNames.get(i)))) {
                    column.setAttribute("minOccurs", "0");
                }
                columnName.setValue(columnNames.get(i));
                column.setAttributeNode(columnType);
                column.setAttributeNode(columnName);
                sequenceElement2.appendChild(column);
            }
            beautifyAndWrite(fileOutputStream, document);
        } catch (final ParserConfigurationException e) {
            throw new IOException("Failed to configure the parser.");
        }
    }

    @Override
    public void writeDatabaseFile(final Database database) throws IOException {
        final Path databaseFilePath = Path.of(database.getURL());
        try (final FileOutputStream fileOutputStream = new FileOutputStream(databaseFilePath.toFile())) {
            final Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
            final Element root = document.createElement("Database");
            root.setAttribute("name", String.valueOf(databaseFilePath.getParent().getFileName()));
            document.appendChild(root);

            final List<Table> tables = database.getTables();
            for (final Table table : tables) {
                final Element tableElement = document.createElement("Table");
                tableElement.setAttribute("name", table.getName());

                final Element tableFile = document.createElement("pathToTable");
                final String xmlPath = table.getTableFile();
                tableFile.setTextContent(xmlPath);
                tableElement.appendChild(tableFile);

                final Element schemaFile = document.createElement("pathToSchema");
                final String xsdPath = table.getSchemaFile();
                schemaFile.setTextContent(xsdPath);
                tableElement.appendChild(schemaFile);

                root.appendChild(tableElement);
            }

            beautifyAndWrite(fileOutputStream, document);
        } catch (final ParserConfigurationException e) {
            throw new IOException("Failed to configure the parser.");
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
            final DocumentBuilder documentBuilder;
            documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Document document = documentBuilder.newDocument();
            final Element root = document.createElement("blob");
            root.setTextContent(blobValue);
            document.appendChild(root);
            beautifyAndWrite(fileOutputStream, document);
        } catch (final ParserConfigurationException e) {
            throw new IOException("Failed to configure the parser.");
        }
        return blobURL;
    }

}
