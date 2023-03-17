package com.github.jfsql.driver.validation;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion;
import com.networknt.schema.ValidationMessage;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum JsonSchemaValidator implements SchemaValidator {

    INSTANCE;

    private static final Logger logger = LogManager.getLogger(JsonSchemaValidator.class);

    @Override
    public boolean schemaIsValid(final String schemaPath, final String tablePath) {
        final ObjectMapper objectMapper = new ObjectMapper();
        final JsonSchemaFactory schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V6);

        try (final InputStream jsonStream = new FileInputStream(tablePath);
            final InputStream schemaStream = new FileInputStream(schemaPath)) {
            final JsonNode json = objectMapper.readTree(jsonStream);
            final JsonSchema schema = schemaFactory.getSchema(schemaStream);
            final Set<ValidationMessage> validationResult = schema.validate(json);

            if (validationResult.isEmpty()) {
                return true;
            } else {
                logger.error(new ArrayList<>(validationResult));
                return false;
            }
        } catch (final IOException e) {
            e.printStackTrace();
        }
        return false;
    }
}

