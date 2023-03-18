package com.github.jfsql.driver.util;

import java.util.Properties;
import lombok.Getter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Getter
public class PropertiesReader {

    private static final Logger logger = LogManager.getLogger(PropertiesReader.class);
    private boolean schemaValidation = true;
    private boolean statementCaching = true;
    private String persistence = "xml";
    private boolean transactionVersioning = true;

    public PropertiesReader(final Properties properties) {
        if (properties == null) {
            logger.info("properties was null, will use the default values.");
            return;
        }

        readStatementCachingProperty(properties);
        readPersistenceProperty(properties);
        readTransactionVersioningProperty(properties);
        readSchemaValidationProperty(properties);
    }

    public boolean isSchemaValidation() {
        return schemaValidation;
    }

    private void readStatementCachingProperty(final Properties properties) {
        final String statementCachingString = properties.getProperty("statement.caching");
        if (statementCachingString != null) {
            try {
                statementCaching = Boolean.parseBoolean(statementCachingString);
                logger.info("Successfully parsed the value '{}' for the key for 'statement.caching', will use '{}'",
                    statementCachingString, statementCaching);
            } catch (final IllegalArgumentException e) {
                logger.info(
                    "Failed to parse the value '{}' for the key for 'statement.caching', the default value (true) will be used",
                    statementCachingString);
            }
        } else {
            logger.info("No value was found for key 'statement.caching', the default value (true) will be used");
        }
    }

    private void readSchemaValidationProperty(final Properties properties) {
        final String schemaValidationString = properties.getProperty("schema.validation");
        if (schemaValidationString != null) {
            try {
                schemaValidation = Boolean.parseBoolean(schemaValidationString);
                logger.info("Successfully parsed the value '{}' for the key 'schema.validation', will use '{}'",
                    schemaValidationString, schemaValidation);
            } catch (final IllegalArgumentException e) {
                logger.info(
                    "Failed to parse the value '{}' for the key 'schema.validation', the default value (true) will be used",
                    schemaValidationString);
            }
        } else {
            logger.info("No value was found for key 'schema.validation', the default value (true) will be used");
        }
    }

    private void readTransactionVersioningProperty(final Properties properties) {
        final String transactionVersioningString = properties.getProperty("transaction.versioning");
        if (transactionVersioningString != null) {
            try {
                transactionVersioning = Boolean.parseBoolean(transactionVersioningString);
                logger.info("Successfully parsed the value '{}' for the key 'transaction.versioning', will use '{}'",
                    transactionVersioningString, transactionVersioning);
            } catch (final IllegalArgumentException e) {
                logger.info(
                    "Failed to parse the value '{}' for the key 'transaction.versioning', the default value (true) will be used",
                    transactionVersioningString);
            }
        } else {
            logger.info("No value was found for key 'transaction.versioning', the default value (true) will be used");
        }
    }

    private void readPersistenceProperty(final Properties properties) {
        String persistenceString = properties.getProperty("persistence");
        if (persistenceString != null) {
            persistenceString = persistenceString.trim();
            if (persistenceString.equalsIgnoreCase("xml") || persistenceString.equalsIgnoreCase("json")) {
                persistence = persistenceString.toLowerCase();
                logger.info("Successfully parsed the value '{}' for the key 'persistence', will use '{}'",
                    persistenceString, persistence);
            } else {
                logger.info(
                    "Failed to parse the value '{}' for the key 'persistence', will use the default value (xml)",
                    persistenceString);
            }
        } else {
            logger.info("No value was found for key 'persistence', the default value (xml) will be used");
        }
    }

}
