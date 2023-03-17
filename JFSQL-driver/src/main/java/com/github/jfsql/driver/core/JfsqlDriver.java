package com.github.jfsql.driver.core;

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
    private static boolean registered;

    static {
        load();
    }

    private static synchronized void load() {
        if (!registered) {
            registered = true;
            try {
                DriverManager.registerDriver(INSTANCE);
            } catch (final SQLException e) {
                e.printStackTrace();
            }
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
    public Connection connect(String url, final Properties info) throws SQLException {
        if (acceptsURL(url)) {
            url = url.replace("jdbc:jfsql:", "");
            if (!url.endsWith(File.separator)) {
                url += File.separator;
            }
            final Path urlPath = Path.of(url);
            if (urlPath.toFile().isFile()) {
                throw new SQLException("Database is not a directory.");
            }
            return new JfsqlConnection(urlPath);
        }
        throw new SQLException("Couldn't establish connection, because wrong connection string format was used.");
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
