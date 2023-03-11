package com.github.jfsql.driver.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import lombok.experimental.UtilityClass;

@UtilityClass
public class PropertiesReader {

    public String getProperty(final String propertyName) {
        final InputStream inputStream = PropertiesReader.class.getClassLoader()
            .getResourceAsStream("from-pom.properties");
        final Properties properties = new Properties();
        try {
            properties.load(inputStream);
        } catch (final IOException e) {
            e.printStackTrace();
        }
        return properties.getProperty(propertyName);
    }
}
