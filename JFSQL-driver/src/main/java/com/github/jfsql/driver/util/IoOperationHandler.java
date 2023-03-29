package com.github.jfsql.driver.util;

import java.io.IOException;
import org.apache.commons.io.FileUtils;

public class IoOperationHandler {

    public void renameFile(final String source, final String destination) throws IOException {
        FileUtils.moveFile(FileUtils.getFile(source), FileUtils.getFile(destination));
    }

}
