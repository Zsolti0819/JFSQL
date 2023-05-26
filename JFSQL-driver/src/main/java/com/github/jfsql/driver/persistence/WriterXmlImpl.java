package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.exceptions.SchemaValidationException;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.github.jfsql.driver.validation.XmlSchemaValidator;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
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
    private final Transformer transformer;
    private final DocumentBuilder documentBuilder;

    public WriterXmlImpl(final boolean useSchemaValidation) {
        super(useSchemaValidation);
        final TransformerFactory transformerFactory = TransformerFactory.newInstance();
        transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
        transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
        try {
            documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        } catch (final TransformerConfigurationException | ParserConfigurationException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public void writeTable(final Table table) throws IOException, SchemaValidationException {
        final String tableFile = table.getTableFile();
        final FileOutputStream fileOutputStream = new FileOutputStream(tableFile);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

        final String tableName = table.getName();
        final List<Entry> entries = table.getEntries();
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
        final Source source = new DOMSource(document);
        final Result result = new StreamResult(fileOutputStream);
        try {
            transformer.transform(source, result);
        } catch (final TransformerException e) {
            throw new IOException(e);
        } finally {
            fileLock.release();
            fileOutputStream.close();
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
        if (value == null || "null".equals(value)) {
            return;
        }
        final Element row;
        if ("BLOB".equals(type)) {
            row = document.createElement(column);
            row.setTextContent(writeBlob(table, entry, column));
        } else {
            row = document.createElement(column);
            row.setTextContent(value);
        }
        element.appendChild(row);
    }

    @Override
    public void writeSchema(final Table table) throws IOException {
        final String schemaFile = table.getSchemaFile();
        final String tableName = Path.of(schemaFile).getFileName().toString().replace(".xsd", "");
        final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

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

        for (final String column : table.getColumnsAndTypes().keySet()) {
            final Element columnElement = document.createElement("xs:element");
            final Attr columnTypeAttribute = document.createAttribute("type");
            columnTypeAttribute.setValue(DatatypeConverter.convertFromSqlToXs(table.getColumnsAndTypes().get(column)));
            final Attr columnName = document.createAttribute("name");
            if (Boolean.FALSE.equals(table.getNotNullColumns().get(column))) {
                columnElement.setAttribute("minOccurs", "0");
            }
            columnName.setValue(column);
            columnElement.setAttributeNode(columnTypeAttribute);
            columnElement.setAttributeNode(columnName);
            sequenceElement2.appendChild(columnElement);
        }
        final Source source = new DOMSource(document);
        final Result result = new StreamResult(fileOutputStream);
        try {
            transformer.transform(source, result);
        } catch (final TransformerException e) {
            throw new IOException(e);
        } finally {
            fileLock.release();
            fileOutputStream.close();
        }
    }

    @Override
    public void writeDatabaseFile(final Database database) throws IOException {
        final String databaseURL = database.getURL();
        final FileOutputStream fileOutputStream = new FileOutputStream(databaseURL);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

        final Document document = documentBuilder.newDocument();
        final Element root = document.createElement("Database");
        root.setAttribute("name", String.valueOf(Path.of(database.getURL()).getParent().getFileName()));
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
        final Source source = new DOMSource(document);
        final Result result = new StreamResult(fileOutputStream);
        try {
            transformer.transform(source, result);
        } catch (final TransformerException e) {
            throw new IOException(e);
        } finally {
            fileLock.release();
            fileOutputStream.close();
        }
    }

    @Override
    public String writeBlob(final Table table, final Entry entry, final String column) throws IOException {
        final LargeObject largeObject = entry.getColumnsAndBlobs().get(column);
        final String blobURL = largeObject.getURL();
        logger.trace("BLOB URL = {}", blobURL);
        final String blobValue = largeObject.getValue();
        logger.trace("BLOB value = {}", blobValue);
        final Path tableParent = Path.of(table.getTableFile()).getParent();
        final Path blobParent = tableParent.resolve("blob");
        Files.createDirectories(blobParent);
        final FileOutputStream fileOutputStream = new FileOutputStream(blobURL);
        final FileChannel fileChannel = fileOutputStream.getChannel();
        final FileLock fileLock = fileChannel.lock();

        final Document document = documentBuilder.newDocument();
        final Element root = document.createElement("blob");
        root.setTextContent(blobValue);
        document.appendChild(root);
        final Source source = new DOMSource(document);
        final Result result = new StreamResult(fileOutputStream);
        try {
            transformer.transform(source, result);
        } catch (final TransformerException e) {
            throw new IOException(e);
        } finally {
            fileLock.release();
            fileOutputStream.close();
        }
        return blobURL;
    }

}
