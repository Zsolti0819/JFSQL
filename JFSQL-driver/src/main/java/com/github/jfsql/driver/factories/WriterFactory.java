package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import com.github.jfsql.driver.util.PropertiesReader;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class WriterFactory {

    public Writer createWriter(final PropertiesReader propertiesReader) {
        final String type = propertiesReader.getPersistence();
        final boolean useSchemaValidation = propertiesReader.isSchemaValidation();
        if (Objects.equals("xml", type)) {
            if (useSchemaValidation) {
                return new WriterXmlImpl(true);
            } else {
                return new WriterXmlImpl(false);
            }

        } else if (Objects.equals("json", type)) {
            if (useSchemaValidation) {
                return new WriterJsonImpl(true);
            } else {
                return new WriterJsonImpl(false);
            }
        } else {
            throw new IllegalArgumentException("Unknown Writer type '" + type + "'");
        }
    }
}
