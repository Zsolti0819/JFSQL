package com.github.jfsql.driver.util;

import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.RequiredArgsConstructor;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RequiredArgsConstructor
public class BlobFileNameCreator {

    private static final Logger logger = LogManager.getLogger(BlobFileNameCreator.class);
    private final DatabaseManager databaseManager;
    private final PropertiesReader propertiesReader;

    public String getBlobURL() {
        final String URL = databaseManager.getURL();
        final Path pathURL = Path.of(URL);
        final String fileExtension = propertiesReader.getPersistence();
        final List<Integer> fileNumbers = new ArrayList<>();
        final Pattern pattern = Pattern.compile("blob" + "(\\d+)" + "\\." + fileExtension);
        final Collection<File> files = FileUtils.listFiles(pathURL.toFile(), new String[]{fileExtension}, false);
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
