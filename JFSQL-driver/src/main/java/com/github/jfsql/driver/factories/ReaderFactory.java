package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.ReaderJsonImpl;
import com.github.jfsql.driver.persistence.ReaderXmlImpl;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ReaderFactory {

    public Reader createReader(final PropertiesReader propertiesReader) {
        final String type = propertiesReader.getPersistence();
        if (Objects.equals("xml", type)) {
            return new ReaderXmlImpl();
        } else if (Objects.equals("json", type)) {
            return new ReaderJsonImpl();
        } else {
            throw new IllegalArgumentException("Unknown Reader type '" + type + "'");
        }
    }
}
