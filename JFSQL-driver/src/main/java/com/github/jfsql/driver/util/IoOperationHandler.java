package com.github.jfsql.driver.util;

import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import java.io.File;
import java.io.IOException;
import org.apache.commons.io.FileUtils;

public class IoOperationHandler {

    public void renameFile(final String source, final String destination) throws IOException {
        FileUtils.moveFile(FileUtils.getFile(source), FileUtils.getFile(destination));
    }

    public boolean databaseDroppedSuccessfully(final DropDatabaseWrapper statement) {
        return FileUtils.deleteQuietly(new File(statement.getDatabaseUrl()));
    }

}
