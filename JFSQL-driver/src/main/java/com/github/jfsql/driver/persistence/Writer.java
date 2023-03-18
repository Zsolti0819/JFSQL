package com.github.jfsql.driver.persistence;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import java.io.File;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.io.FileUtils;

public abstract class Writer {

    final boolean useSchemaValidation;

    protected Writer(final boolean useSchemaValidation) {
        this.useSchemaValidation = useSchemaValidation;
    }

    public abstract void writeTable(final Table table) throws SQLException;

    public abstract void writeSchema(final Table table) throws SQLException;

    public abstract void writeDatabaseFile(final Database database) throws SQLException;

    abstract Path writeBlob(final Table table, final String value) throws SQLException;

    String incrementFileName(final Path folderPath, final String fileExtension) {
        final List<Integer> fileNumbers = new ArrayList<>();
        final Pattern pattern = Pattern.compile("blob" + "(\\d+)" + "\\." + fileExtension);
        final Collection<File> filesInFolder = FileUtils.listFiles(folderPath.toFile(), new String[]{fileExtension},
            false);
        for (final File file : filesInFolder) {
            final String fileName = file.getName();
            final Matcher matcher = pattern.matcher(fileName);
            if (matcher.matches()) {
                final int fileNumber = Integer.parseInt(matcher.group(1));
                fileNumbers.add(fileNumber);
            }
        }

        final int nextFileNumber = fileNumbers.stream()
            .max(Integer::compareTo)
            .map(num -> num + 1)
            .orElse(1);

        return "blob" + nextFileNumber + "." + fileExtension;
    }

}
