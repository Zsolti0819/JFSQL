package com.github.jfsql.driver.util;

import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import java.io.File;
import org.apache.commons.io.FileUtils;

public class IoOperationHandler {

    public boolean databaseDroppedSuccessfully(final DropDatabaseWrapper statement) {
        return FileUtils.deleteQuietly(new File(statement.getDatabaseURL()));
    }

}
