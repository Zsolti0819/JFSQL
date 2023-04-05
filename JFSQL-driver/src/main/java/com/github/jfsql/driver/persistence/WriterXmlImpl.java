package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.LargeObject;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.github.jfsql.driver.validation.SchemaValidationException;
import com.github.jfsql.driver.validation.XmlSchemaValidator;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
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
            final Result result = new StreamResult(fileOutputStream);
            transformer.transform(source, result);
        } catch (final TransformerException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void writeTable(final Table table) throws IOException, SchemaValidationException {
        logger.trace("table = {}", table);
        logger.trace("table entries = {}", table.getEntries());
        final String tableFile = table.getTableFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(tableFile);
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            final FileLock lock = fileChannel.tryLock();
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
            lock.release();
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
        logger.trace("table = {}", schema);
        final String tableName = String.valueOf(Path.of(schema.getSchemaFile()).getFileName()).replace(".xsd", "");
        final String schemaFile = schema.getSchemaFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile);
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            final FileLock lock = fileChannel.tryLock();
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
            lock.release();
        } catch (final ParserConfigurationException e) {
            throw new IOException("Failed to configure the parser.");
        }
    }

    @Override
    public void writeDatabaseFile(final Database database) throws IOException {
        logger.trace("database = {}", database);
        logger.trace("database tables = {}", database.getTables());
        final Path databaseFilePath = database.getURL();
        final String databaseFileParentPath = String.valueOf(databaseFilePath.getParent());
        final Path databaseFolderName = Path.of(databaseFileParentPath);
        try (final FileOutputStream fileOutputStream = new FileOutputStream(databaseFilePath.toFile());
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            final FileLock lock = fileChannel.tryLock();
            final Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
            final Element root = document.createElement("Database");
            root.setAttribute("name", String.valueOf(databaseFolderName.getFileName()));
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
            lock.release();
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
        final Path blobParent = Path.of(tableParent + File.separator + "blob");
        Files.createDirectories(blobParent);

        try (final FileOutputStream fileOutputStream = new FileOutputStream(blobURL);
            final FileChannel fileChannel = fileOutputStream.getChannel()) {
            final FileLock lock = fileChannel.tryLock();
            final DocumentBuilder documentBuilder;
            documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Document document = documentBuilder.newDocument();
            final Element root = document.createElement("blob");
            root.setTextContent(blobValue);
            document.appendChild(root);
            beautifyAndWrite(fileOutputStream, document);
            lock.release();
        } catch (final ParserConfigurationException e) {
            throw new IOException("Failed to configure the parser.");
        }
        return blobURL;
    }

}
