package com.github.jfsql.driver.util;

import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.experimental.UtilityClass;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@UtilityClass
public class BlobFileNameCreator {

    private static final Logger logger = LogManager.getLogger(BlobFileNameCreator.class);

    public String getBlobURL(final DatabaseManager databaseManager, final IoOperationHandler ioOperationHandler,
        final Writer writer) {
        final String URL = databaseManager.getURL();
        final Path pathURL = Path.of(URL);
        final String fileExtension = writer instanceof WriterXmlImpl ? "xml" : "json";
        final List<Integer> fileNumbers = new ArrayList<>();
        final Pattern pattern = Pattern.compile("blob" + "(\\d+)" + "\\." + fileExtension);
        final Collection<File> files = ioOperationHandler.listFiles(pathURL, fileExtension);
        for (final File file : files) {
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

        final String nextBlobName =
            pathURL + File.separator + "blob" + File.separator + "blob" + nextFileNumber + "." + fileExtension;
        logger.debug("nextBlobName = {}", nextBlobName);
        return nextBlobName;
    }

}
