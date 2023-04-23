package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.Persistence;
import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import lombok.experimental.UtilityClass;

@UtilityClass
public class WriterFactory {

    public Writer createWriter(final PropertiesReader propertiesReader) {
        final Persistence type = propertiesReader.getPersistence();
        final boolean useSchemaValidation = propertiesReader.isSchemaValidation();
        switch (type) {
            case XML:
                if (useSchemaValidation) {
                    return new WriterXmlImpl(true);
                } else {
                    return new WriterXmlImpl(false);
                }
            case JSON:
                if (useSchemaValidation) {
                    return new WriterJsonImpl(true);
                } else {
                    return new WriterJsonImpl(false);
                }
            default:
                throw new IllegalArgumentException("Unknown Writer type '" + type);
        }
    }
}
