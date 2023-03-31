package com.github.jfsql.driver.core;

import com.github.jfsql.driver.config.PropertiesReader;
import java.io.File;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.DriverPropertyInfo;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Properties;
import java.util.logging.Logger;
import lombok.Getter;

@Getter
public class JfsqlDriver implements Driver {

    private static final Driver INSTANCE = new JfsqlDriver();

    static {
        try {
            DriverManager.registerDriver(INSTANCE);
        } catch (final SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean acceptsURL(final String url) {
        if (url == null) {
            return false;
        }
        return url.startsWith("jdbc:jfsql:");
    }

    @Override
    public Connection connect(final String url, final Properties info) throws SQLException {
        if (!acceptsURL(url)) {
            throw new SQLException("Couldn't establish connection, because wrong connection string format was used.");
        }
        String urlWithoutPrefix = url.replace("jdbc:jfsql:", "");
        if (!urlWithoutPrefix.endsWith(File.separator)) {
            urlWithoutPrefix += File.separator;
        }
        final Path urlPath = Path.of(urlWithoutPrefix);
        if (urlPath.toFile().isFile()) {
            throw new SQLException("Database is not a directory.");
        }
        final PropertiesReader propertiesReader = new PropertiesReader(info);
        return new JfsqlConnection(urlPath, propertiesReader);
    }

    @Override
    public int getMajorVersion() {
        return 1;
    }

    @Override
    public int getMinorVersion() {
        return 0;
    }

    @Override
    public boolean jdbcCompliant() {
        return false;
    }

    // Unsupported operations

    @Override
    public DriverPropertyInfo[] getPropertyInfo(final String url, final Properties info)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Logger getParentLogger() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }
}
