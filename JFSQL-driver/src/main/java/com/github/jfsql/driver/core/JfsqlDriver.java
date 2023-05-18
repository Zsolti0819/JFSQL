package com.github.jfsql.driver.core;

import com.github.jfsql.driver.cache.statement.Cache;
import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.factories.CacheFactory;
import com.github.jfsql.driver.factories.DatabaseManagerFactory;
import com.github.jfsql.driver.factories.ReaderFactory;
import com.github.jfsql.driver.factories.TransactionManagerFactory;
import com.github.jfsql.driver.factories.WriterFactory;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.services.AlterTableService;
import com.github.jfsql.driver.services.CreateTableService;
import com.github.jfsql.driver.services.DeleteService;
import com.github.jfsql.driver.services.DropTableService;
import com.github.jfsql.driver.services.InsertService;
import com.github.jfsql.driver.services.PreparedStatementCreator;
import com.github.jfsql.driver.services.SelectService;
import com.github.jfsql.driver.services.StatementServiceManager;
import com.github.jfsql.driver.services.UpdateService;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.core.Parser;
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
    public boolean acceptsURL(final String URL) {
        if (URL == null) {
            return false;
        }
        return URL.startsWith("jdbc:jfsql:");
    }

    @Override
    public Connection connect(final String URL, final Properties info) throws SQLException {
        if (!acceptsURL(URL)) {
            throw new SQLException("Couldn't establish connection, because wrong connection string format was used.");
        }

        final PropertiesReader propertiesReader = new PropertiesReader(info);
        final SemanticValidator semanticValidator = new SemanticValidator();
        final Cache cache = CacheFactory.createCache(propertiesReader);
        final Reader reader = ReaderFactory.createReader(propertiesReader);
        final Writer writer = WriterFactory.createWriter(propertiesReader);
        final DatabaseManager databaseManager = DatabaseManagerFactory
            .createDatabaseManager(propertiesReader, URL, reader, writer);
        final Database database = databaseManager.getDatabase();
        final TransactionManager transactionManager = TransactionManagerFactory
            .createTransactionManager(propertiesReader, databaseManager, reader, writer);

        // Classes used in statement services
        final Parser parser = new Parser();
        final IoOperationHandler ioOperationHandler = new IoOperationHandler();
        final PreparedStatementCreator preparedStatementCreator = new PreparedStatementCreator(database);

        // Specific statement services
        final AlterTableService alterTableService = new AlterTableService(database, transactionManager,
            semanticValidator, reader);
        final CreateTableService createTableService = new CreateTableService(database, transactionManager,
            semanticValidator, reader);
        final InsertService insertService = new InsertService(database, transactionManager, semanticValidator, reader,
            preparedStatementCreator);
        final SelectService selectService = new SelectService(database, semanticValidator, reader);
        final UpdateService updateService = new UpdateService(database, transactionManager, semanticValidator, reader,
            preparedStatementCreator);
        final DeleteService deleteService = new DeleteService(transactionManager, database, semanticValidator, reader);
        final DropTableService dropTableService = new DropTableService(database, transactionManager, semanticValidator,
            reader);

        final StatementServiceManager statementServiceManager = StatementServiceManager.builder()
            .cache(cache)
            .parser(parser)
            .preparedStatementCreator(preparedStatementCreator)
            .alterTableService(alterTableService)
            .createTableService(createTableService)
            .insertService(insertService)
            .selectService(selectService)
            .updateService(updateService)
            .deleteService(deleteService)
            .dropTableService(dropTableService)
            .ioOperationHandler(ioOperationHandler)
            .semanticValidator(semanticValidator)
            .build();

        return JfsqlConnection.builder()
            .writer(writer)
            .databaseManager(databaseManager)
            .transactionManager(transactionManager)
            .statementServiceManager(statementServiceManager)
            .build();
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
    public DriverPropertyInfo[] getPropertyInfo(final String URL, final Properties info)
        throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }

    @Override
    public Logger getParentLogger() throws SQLFeatureNotSupportedException {
        throw new SQLFeatureNotSupportedException();
    }
}
