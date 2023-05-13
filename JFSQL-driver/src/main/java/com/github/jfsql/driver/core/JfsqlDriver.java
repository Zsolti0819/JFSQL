package com.github.jfsql.driver.core;

import com.github.jfsql.driver.cache.statement.Cache;
import com.github.jfsql.driver.config.PropertiesReader;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.TransactionManager;
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
import com.github.jfsql.driver.services.SelectService;
import com.github.jfsql.driver.services.StatementServiceManager;
import com.github.jfsql.driver.services.UpdateService;
import com.github.jfsql.driver.util.BlobFileNameCreator;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.util.PreparedStatementCreator;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
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
            .createDatabaseManager(propertiesReader, URL, semanticValidator, reader, writer);
        final TransactionManager transactionManager = TransactionManagerFactory
            .createTransactionManager(propertiesReader, databaseManager, reader, writer);

        // Classes used in statement services
        final Parser parser = new Parser();
        final IoOperationHandler ioOperationHandler = new IoOperationHandler();
        final ColumnToTypeMapper columnToTypeMapper = new ColumnToTypeMapper();
        final WhereConditionSolver whereConditionSolver = new WhereConditionSolver();
        final TableFinder tableFinder = new TableFinder(databaseManager);
        final PreparedStatementCreator preparedStatementCreator = new PreparedStatementCreator(tableFinder);

        // Specific statement services
        final AlterTableService alterTableService = new AlterTableService(tableFinder, databaseManager,
            transactionManager, semanticValidator, reader);
        final CreateTableService createTableService = new CreateTableService(databaseManager, transactionManager,
            semanticValidator, reader);
        final InsertService insertService = new InsertService(tableFinder, transactionManager,
            semanticValidator,
            reader, preparedStatementCreator);
        final SelectService selectService = new SelectService(tableFinder, semanticValidator, columnToTypeMapper,
            whereConditionSolver, reader);
        final UpdateService updateService = new UpdateService(tableFinder, transactionManager, semanticValidator,
            columnToTypeMapper, whereConditionSolver, reader, preparedStatementCreator);
        final DeleteService deleteService = new DeleteService(tableFinder, transactionManager,
            semanticValidator,
            whereConditionSolver, reader);
        final DropTableService dropTableService = new DropTableService(tableFinder, databaseManager, transactionManager,
            semanticValidator, reader);

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
            .tableFinder(tableFinder)
            .semanticValidator(semanticValidator)
            .columnToTypeMapper(columnToTypeMapper)
            .whereConditionSolver(whereConditionSolver)
            .build();

        // Only used in JfsqlPreparedStatement
        final BlobFileNameCreator blobFileNameCreator = new BlobFileNameCreator(databaseManager, ioOperationHandler,
            propertiesReader);

        return JfsqlConnection.builder()
            .blobFileNameCreator(blobFileNameCreator)
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
