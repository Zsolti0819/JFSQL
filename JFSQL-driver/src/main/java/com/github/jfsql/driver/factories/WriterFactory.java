package com.github.jfsql.driver.factories;

import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterJsonImpl;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class WriterFactory {

    public Writer createWriter(final String type) {
        if (Objects.equals("xml", type)) {
            return new WriterXmlImpl();
        } else if (Objects.equals("json", type)) {
            return new WriterJsonImpl();
        } else {
            throw new IllegalArgumentException("Unknown Writer type");
        }
    }
}
