package com.github.jfsql.driver.validation;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import java.io.File;
import java.io.IOException;

public enum XmlSchemaValidator implements SchemaValidator {

    INSTANCE;

    private static final Logger logger = LogManager.getLogger(XmlSchemaValidator.class);

    @Override
    public boolean schemaIsValid(final String schemaPath, final String tablePath) {
        try {
            final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            schemaFactory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            final Schema schema = schemaFactory.newSchema(new File(schemaPath));
            final Validator validator = schema.newValidator();
            validator.validate(new StreamSource(new File(tablePath)));
        } catch (final IOException | SAXException e) {
            logger.error(e.getMessage());
            return false;
        }
        return true;
    }
}
