package com.github.jfsql.driver.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.github.jfsql.driver.TestUtils;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.persistence.WriterXmlImpl;
import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class BlobFileNameCreatorTest {

    @Mock
    private DatabaseManager databaseManager;

    @Mock
    private IoOperationHandler ioOperationHandler;

    @Spy
    private Writer writer = new WriterXmlImpl(false);


    @Test
    public void getBlobURL() {
        when(databaseManager.getURL()).thenReturn(String.valueOf(TestUtils.XML_DATABASE_PATH));

        final File mockFile1 = mock(java.io.File.class);
        when(mockFile1.getName()).thenReturn("blob1.xml");
        final File mockFile2 = mock(java.io.File.class);
        when(mockFile2.getName()).thenReturn("blob2.xml");
        final File mockFile3 = mock(java.io.File.class);
        when(mockFile3.getName()).thenReturn("blob3.xml");
        final File mockFile4 = mock(java.io.File.class);
        when(mockFile4.getName()).thenReturn("blob4.xml");

        when(ioOperationHandler.listFiles(any(), any())).thenReturn(
            Arrays.asList(mockFile1, mockFile2, mockFile3, mockFile4)
        );

        final String nextBlobUrl = String.valueOf(
            Path.of(String.valueOf(TestUtils.XML_DATABASE_PATH), "blob", "blob5.xml"));
        assertEquals(nextBlobUrl, BlobFileNameCreator.getBlobURL(databaseManager, ioOperationHandler, writer));
    }
}
