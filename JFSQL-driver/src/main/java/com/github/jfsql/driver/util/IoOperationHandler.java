package com.github.jfsql.driver.util;

import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import java.io.File;
import java.nio.file.Path;
import java.util.Collection;
import org.apache.commons.io.FileUtils;

public class IoOperationHandler {

    public boolean databaseDroppedSuccessfully(final DropDatabaseWrapper statement) {
        return FileUtils.deleteQuietly(new File(statement.getDatabaseURL()));
    }

    public Collection<File> listFiles(final Path pathURL, final String fileExtension) {
        return FileUtils.listFiles(pathURL.toFile(), new String[]{fileExtension}, false);
    }

}
