package com.github.jfsql.driver.config;

import java.util.Properties;
import lombok.Getter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Getter
public class PropertiesReader {

    private static final Logger logger = LogManager.getLogger(PropertiesReader.class);
    private String persistence = "xml";
    private String transactionVersioning = "jgit";
    private boolean schemaValidation = true;
    private boolean statementCaching = true;


    public PropertiesReader(final Properties properties) {
        if (properties == null) {
            logger.info("Properties was null, default config will be used: "
                + "persistence = {}, "
                + "transaction.versioning = {}, "
                + "schema.validation = {}, "
                + "statement.caching = {}", persistence, transactionVersioning, schemaValidation, statementCaching);
            return;
        }

        readPersistenceProperty(properties);
        readTransactionVersioningProperty(properties);
        readSchemaValidationProperty(properties);
        readStatementCachingProperty(properties);

        logger.info("The following config will be used: "
            + "persistence = {}, "
            + "transaction.versioning = {}, "
            + "schema.validation = {}, "
            + "statement.caching = {}", persistence, transactionVersioning, schemaValidation, statementCaching);
    }

    private void readPersistenceProperty(final Properties properties) {
        String persistenceString = properties.getProperty("persistence");
        if (persistenceString != null) {
            persistenceString = persistenceString.trim();
            if (persistenceString.equalsIgnoreCase("xml") || persistenceString.equalsIgnoreCase("json")) {
                persistence = persistenceString.toLowerCase();
                logger.trace("Successfully parsed the value '{}' for the key 'persistence', will use '{}'",
                    persistenceString, persistence);
            } else {
                logger.trace(
                    "Failed to parse the value '{}' for the key 'persistence', will use the default value '{}'",
                    persistenceString, persistence);
            }
        } else {
            logger.trace("No value was found for key 'persistence', the default value '{}' will be used",
                persistence);
        }
    }

    private void readTransactionVersioningProperty(final Properties properties) {
        String transactionVersioningString = properties.getProperty("transaction.versioning");
        if (transactionVersioningString != null) {
            transactionVersioningString = transactionVersioningString.trim();
            if (transactionVersioningString.equalsIgnoreCase("jgit") || transactionVersioningString.equalsIgnoreCase(
                "none")) {
                transactionVersioning = transactionVersioningString.toLowerCase();
                logger.trace("Successfully parsed the value '{}' for the key 'transaction.versioning', will use '{}'",
                    transactionVersioningString, transactionVersioning);
            } else {
                logger.trace(
                    "Failed to parse the value '{}' for the key 'transaction.versioning', the default value '{}' will be used",
                    transactionVersioningString, transactionVersioning);
            }
        } else {
            logger.trace("No value was found for key 'transaction.versioning', the default value '{}' will be used",
                transactionVersioning);
        }
    }

    private void readStatementCachingProperty(final Properties properties) {
        final String statementCachingString = properties.getProperty("statement.caching");
        if (statementCachingString != null) {
            try {
                statementCaching = Boolean.parseBoolean(statementCachingString);
                logger.trace("Successfully parsed the value '{}' for the key for 'statement.caching', will use '{}'",
                    statementCachingString, statementCaching);
            } catch (final IllegalArgumentException e) {
                logger.trace(
                    "Failed to parse the value '{}' for the key for 'statement.caching', the default value '{}' will be used",
                    statementCachingString, statementCaching);
            }
        } else {
            logger.trace("No value was found for key 'statement.caching', the default value '{}' will be used",
                statementCaching);
        }
    }

    private void readSchemaValidationProperty(final Properties properties) {
        final String schemaValidationString = properties.getProperty("schema.validation");
        if (schemaValidationString != null) {
            try {
                schemaValidation = Boolean.parseBoolean(schemaValidationString);
                logger.trace("Successfully parsed the value '{}' for the key 'schema.validation', will use '{}'",
                    schemaValidationString, schemaValidation);
            } catch (final IllegalArgumentException e) {
                logger.trace(
                    "Failed to parse the value '{}' for the key 'schema.validation', the default value '{}' will be used",
                    schemaValidationString, schemaValidation);
            }
        } else {
            logger.trace("No value was found for key 'schema.validation', the default value '{}' will be used",
                schemaValidation);
        }
    }

}
