package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.Persistence;
import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ReaderFactory {

    public Reader createReader(final PropertiesReader propertiesReader) {
        final Persistence type = propertiesReader.getPersistence();
        switch (type) {
            case XML:
                return new ReaderXmlImpl();
            case JSON:
                return new ReaderJsonImpl();
            default:
                throw new IllegalArgumentException("Unknown Reader type '" + type);
        }
    }
}
