package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Entry;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.validation.SchemaValidationException;
import java.io.IOException;

public abstract class Writer {

    final boolean useSchemaValidation;

    protected Writer(final boolean useSchemaValidation) {
        this.useSchemaValidation = useSchemaValidation;
    }

    public abstract void writeTable(final Table table) throws IOException, SchemaValidationException;

    public abstract void writeSchema(final Table table) throws IOException;

    public abstract void writeDatabaseFile(final Database database) throws IOException;

    abstract String writeBlob(final Table table, final Entry entry, final String column) throws IOException;

}
