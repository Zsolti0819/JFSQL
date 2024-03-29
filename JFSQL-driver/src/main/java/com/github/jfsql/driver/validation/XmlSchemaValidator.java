package com.github.jfsql.driver.validation;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.xml.sax.SAXException;

public enum XmlSchemaValidator implements SchemaValidator {

    INSTANCE;

    private static final Logger logger = LogManager.getLogger(XmlSchemaValidator.class);

    @Override
    public boolean schemaIsValid(final String schemaPath, final String tablePath) {
        try {
            final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");

            final File schemaFile = new File(schemaPath);
            final File tableFile = new File(tablePath);

            try (final FileInputStream tableInputStream = new FileInputStream(tableFile)) {
                final Schema schema = schemaFactory.newSchema(schemaFile);
                final Validator validator = schema.newValidator();
                validator.validate(new StreamSource(tableInputStream));
            }
        } catch (final IOException | SAXException e) {
            logger.error(e.getMessage());
            return false;
        }
        return true;
    }
}
