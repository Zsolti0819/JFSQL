package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.util.DatatypeConverter;
import com.github.jfsql.driver.validation.XmlSchemaValidator;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;

public class WriterXmlImpl extends Writer {

    private static final XmlSchemaValidator XML_SCHEMA_VALIDATOR = XmlSchemaValidator.INSTANCE;

    @Override
    public String getFileExtension() {
        return "xml";
    }

    @Override
    public String getSchemaFileExtension() {
        return "xsd";
    }

    private void beautifyAndWrite(final FileOutputStream fileOutputStream, final Document document)
            throws SQLException {
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
            throw new SQLException("Failed to transform the document.\n" + e.getMessage());
        }
    }

    @Override
    public void writeTable(final Table table) throws SQLException {
        final String tableFile = table.getTableFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(tableFile);
             final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final String tableName = table.getName();
            final List<Entry> entries = table.getEntries();

            final DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Document document = documentBuilder.newDocument();
            final Element root = document.createElement(tableName);

            for (final Entry entry : entries) {
                final String[] columns = entry.getColumns();
                final String[] values = entry.getValues();
                final Element element = document.createElement("Entry");
                for (int i = 0; i < columns.length; i++) {
                    checkTypeAndValueThenAddProperty(table, document, columns, values, element, i);
                }
                root.appendChild(element);
            }
            document.appendChild(root);
            beautifyAndWrite(fileOutputStream, document);
        } catch (final IOException | ParserConfigurationException e) {
            throw new SQLException("Failed to write the table\n" + e.getMessage());
        }
        if (USE_SCHEMA_VALIDATION.equals(true)) {
            final String schemaFile = table.getSchemaFile();
            final boolean isValid = XML_SCHEMA_VALIDATOR.schemaIsValid(schemaFile, tableFile);
            if (!isValid) {
                throw new SQLException("\"" + tableFile + "\" is not valid against \"" + schemaFile + "\"");
            }
        }
        try (final Git git = Git.open(Path.of(table.getTableFile()).getParent().toFile())) {
            git.add().addFilepattern(String.valueOf(Path.of(tableFile).getFileName())).call();
        } catch (final IOException | GitAPIException e) {
            throw new SQLException("Git operation failed.\n" + e.getMessage());
        }
    }

    private void checkTypeAndValueThenAddProperty(final Table table, final Document document, final String[] columns, final String[] values, final Element element, final int index) throws SQLException {
        if (values[index] != null) {
            Element row = null;
            if (Objects.equals(table.getTypes()[index], "BLOB")) {
                final Path blobPath = writeBlob(table, values[index]);
                if (!Objects.equals(String.valueOf(blobPath), "null")) {
                    row = document.createElement(columns[index]);
                    row.setTextContent(String.valueOf(blobPath));
                }
            } else {
                row = document.createElement(columns[index]);
                row.setTextContent(values[index]);
            }
            if (row != null) {
                element.appendChild(row);
            }
        }
    }

    @Override
    public void writeSchema(final Table table) throws SQLException {
        final String tableName = table.getName();
        final String schemaFile = table.getSchemaFile();
        try (final FileOutputStream fileOutputStream = new FileOutputStream(schemaFile);
             final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final String[] columnNames = table.getColumns();
            final String[] columnTypes = table.getTypes();
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

            for (int i = 0; i < columnTypes.length; i++) {
                final Element column = document.createElement("xs:element");
                final Attr columnType = document.createAttribute("type");
                columnType.setValue(DatatypeConverter.convertFromSqlToXs(columnTypes[i]));
                final Attr columnName = document.createAttribute("name");
                if (Boolean.FALSE.equals(table.getNotNullColumns().get(columnNames[i]))) {
                    column.setAttribute("minOccurs", "0");
                }
                columnName.setValue(columnNames[i]);
                column.setAttributeNode(columnType);
                column.setAttributeNode(columnName);
                sequenceElement2.appendChild(column);
            }
            beautifyAndWrite(fileOutputStream, document);
        } catch (final IOException | ParserConfigurationException e) {
            throw new SQLException("Failed to write the schema.\n" + e.getMessage());
        }
        try (final Git git = Git.open(Path.of(table.getTableFile()).getParent().toFile())) {
            git.add().addFilepattern(String.valueOf(Path.of(schemaFile).getFileName())).call();
        } catch (final IOException | GitAPIException e) {
            throw new SQLException("Git operation failed.\n" + e.getMessage());
        }
    }

    @Override
    public void writeDatabaseFile(final Database database) throws SQLException {
        final Path databaseFilePath = database.getUrl();
        final String databaseFileParentPath = String.valueOf(databaseFilePath.getParent());
        final Path databaseFolderName = Path.of(databaseFileParentPath);
        try (final FileOutputStream fileOutputStream = new FileOutputStream(databaseFilePath.toFile());
             final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
            final Element root = document.createElement("Database");
            root.setAttribute("name", String.valueOf(databaseFolderName.getFileName()));
            document.appendChild(root);

            final List<Table> tables = database.getTables();
            if (tables != null) {
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
            }
            beautifyAndWrite(fileOutputStream, document);
        } catch (final IOException | ParserConfigurationException e) {
            throw new SQLException("Failed to write the database file.\n" + e.getMessage());
        }
        try (final Git git = Git.open(databaseFolderName.toFile())) {
            git.add().addFilepattern(String.valueOf(databaseFilePath.getFileName())).call();
        } catch (final IOException | GitAPIException e) {
            throw new SQLException("Git operation failed.\n" + e.getMessage());
        }
    }

    @Override
    Path writeBlob(final Table table, final String value) throws SQLException {
        if (Objects.equals(value, "null")) {
            return null;
        }
        final Path tableParent = Path.of(table.getTableFile()).getParent();
        final Path blobParent = Path.of(tableParent + File.separator + "blob");
        try {
            Files.createDirectories(blobParent);
        } catch (final IOException e) {
            throw new SQLException("Failed to create directory for BLOBs.\n" + e.getMessage());
        }
        final String newBlobName = incrementFileName(blobParent, getFileExtension());
        final Path blobPath = Path.of(blobParent + File.separator + newBlobName);
        try (final FileOutputStream fileOutputStream = new FileOutputStream(blobPath.toFile());
             final FileChannel fileChannel = fileOutputStream.getChannel()) {
            fileChannel.tryLock();
            final DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Document document = documentBuilder.newDocument();
            final Element root = document.createElement("blob");
            root.setTextContent(value);
            document.appendChild(root);
            beautifyAndWrite(fileOutputStream, document);
        } catch (final IOException | ParserConfigurationException e) {
            throw new SQLException("Failed to write the blob\n" + e.getMessage());
        }
        try (final Git git = Git.open(Path.of(table.getTableFile()).getParent().toFile())) {
            git.add().addFilepattern(String.valueOf(blobPath.getFileName())).call();
        } catch (final IOException | GitAPIException e) {
            throw new SQLException("Git operation failed.\n" + e.getMessage());
        }
        return blobPath;
    }

}
