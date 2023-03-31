package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.db.DatabaseManager;
import com.github.jfsql.driver.db.TransactionManager;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.FileNameCreator;
import com.github.jfsql.driver.util.IoOperationHandler;
import com.github.jfsql.driver.util.PreparedStatementCreator;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.core.Parser;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import com.github.jfsql.parser.dto.BaseStatement;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import com.github.jfsql.parser.dto.DeleteWrapper;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import com.github.jfsql.parser.dto.DropTableWrapper;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.SelectWrapper;
import com.github.jfsql.parser.dto.TypeOfStatement;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.sql.ResultSet;
import java.sql.SQLException;
import lombok.Data;

@Data
public class StatementServiceManager {

    static final Object lock = new Object();
    private final Cache cache;
    private final Parser parser;
    private final PreparedStatementCreator preparedStatementCreator;

    private final AlterTableService alterTableService;
    private final CreateDatabaseService createDatabaseService;
    private final DropDatabaseService dropDatabaseService;
    private final CreateTableService createTableService;
    private final InsertService insertService;
    private final SelectService selectService;
    private final UpdateService updateService;
    private final DeleteService deleteService;
    private final DropTableService dropTableService;

    private ResultSet resultSet;
    private int updateCount = 0;
    private Object[] parameters;

    public StatementServiceManager(final DatabaseManager databaseManager, final Cache cache,
        final TransactionManager transactionManager, final Reader reader) {
        this.cache = cache;

        final IoOperationHandler ioOperationHandler = new IoOperationHandler();
        final FileNameCreator fileNameCreator = new FileNameCreator(reader);
        final TableFinder tableFinder = new TableFinder(databaseManager);
        final SemanticValidator semanticValidator = new SemanticValidator();
        final ColumnToTypeMapper columnToTypeMapper = new ColumnToTypeMapper();
        final WhereConditionSolver whereConditionSolver = new WhereConditionSolver();

        parser = new Parser();
        preparedStatementCreator = new PreparedStatementCreator(tableFinder, this);

        alterTableService = new AlterTableService(tableFinder, databaseManager, transactionManager, semanticValidator,
            ioOperationHandler, fileNameCreator, reader);
        createDatabaseService = new CreateDatabaseService(databaseManager, semanticValidator, fileNameCreator, reader);
        dropDatabaseService = new DropDatabaseService(databaseManager, semanticValidator, reader);
        createTableService = new CreateTableService(databaseManager, transactionManager, semanticValidator,
            fileNameCreator);
        insertService = new InsertService(tableFinder, transactionManager, semanticValidator, reader);
        selectService = new SelectService(tableFinder, semanticValidator, columnToTypeMapper,
            whereConditionSolver,
            reader);
        updateService = new UpdateService(tableFinder, transactionManager, semanticValidator,
            columnToTypeMapper,
            whereConditionSolver, reader);
        deleteService = new DeleteService(tableFinder, transactionManager, semanticValidator,
            whereConditionSolver,
            reader);
        dropTableService = new DropTableService(tableFinder, databaseManager, transactionManager,
            semanticValidator,
            reader);
    }

    private BaseStatement getFromCacheOrParseStatement(final String sql) {
        final BaseStatement statement;
        if (cache.getCachedStatements().containsKey(sql)) {
            statement = cache.getCachedStatements().get(sql);
        } else {
            statement = parser.parse(sql);
            cache.getCachedStatements().put(sql, statement);
        }
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        return statement;
    }

    // Statements

    public ResultSet executeQuery(final String sql, final boolean isPreparedStatement) throws SQLException {
        synchronized (lock) {
            final BaseStatement statement = getFromCacheOrParseStatement(sql);
            if (!(TypeOfStatement.SELECT.equals(statement.getTypeOfStatement()))) {
                throw new SQLException("Method not supported for this statement.");
            }
            resultSet = selectService.selectFromTable(isPreparedStatement ?
                preparedStatementCreator.getPreparedSelectStatement((SelectWrapper) statement) :
                (SelectWrapper) statement);
            return resultSet;
        }
    }

    public int executeUpdate(final String sql, final boolean isPreparedStatement) throws SQLException {
        synchronized (lock) {
            final BaseStatement statement = getFromCacheOrParseStatement(sql);
            final TypeOfStatement statementType = statement.getTypeOfStatement();
            switch (statementType) {
                case ALTER_TABLE:
                    updateCount = alterTableService.alterTable((AlterTableWrapper) statement);
                    break;
                case CREATE_DATABASE:
                    updateCount = createDatabaseService.createDatabase((CreateDatabaseWrapper) statement);
                    break;
                case CREATE_TABLE:
                    updateCount = createTableService.createTable((CreateTableWrapper) statement);
                    break;
                case DELETE:
                    updateCount = deleteService.deleteFromTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedDeleteStatement((DeleteWrapper) statement) :
                        (DeleteWrapper) statement);
                    break;
                case DROP_DATABASE:
                    updateCount = dropDatabaseService.dropDatabase((DropDatabaseWrapper) statement);
                    break;
                case DROP_TABLE:
                    updateCount = dropTableService.dropTable((DropTableWrapper) statement);
                    break;
                case INSERT:
                    updateCount = insertService.insertIntoTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedInsertStatement((InsertWrapper) statement) :
                        (InsertWrapper) statement);
                    break;
                case SELECT:
                    throw new SQLException("Method not supported.");
                case UPDATE:
                    updateCount = updateService.updateTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedUpdateStatement((UpdateWrapper) statement) :
                        (UpdateWrapper) statement);
                    break;
                default:
                    throw new SQLException("Statement type '" + statementType + "' is not supported.");
            }
            return updateCount;
        }
    }

    public boolean execute(final String sql, final boolean isPreparedStatement) throws SQLException {
        synchronized (lock) {
            final BaseStatement statement = getFromCacheOrParseStatement(sql);
            final TypeOfStatement statementType = statement.getTypeOfStatement();
            switch (statementType) {
                case ALTER_TABLE:
                    updateCount = alterTableService.alterTable((AlterTableWrapper) statement);
                    break;
                case CREATE_DATABASE:
                    updateCount = createDatabaseService.createDatabase((CreateDatabaseWrapper) statement);
                    break;
                case CREATE_TABLE:
                    updateCount = createTableService.createTable((CreateTableWrapper) statement);
                    break;
                case DELETE:
                    updateCount = deleteService.deleteFromTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedDeleteStatement((DeleteWrapper) statement) :
                        (DeleteWrapper) statement);
                    break;
                case DROP_DATABASE:
                    updateCount = dropDatabaseService.dropDatabase((DropDatabaseWrapper) statement);
                    break;
                case DROP_TABLE:
                    updateCount = dropTableService.dropTable((DropTableWrapper) statement);
                    break;
                case INSERT:
                    updateCount = insertService.insertIntoTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedInsertStatement((InsertWrapper) statement) :
                        (InsertWrapper) statement);
                    break;
                case SELECT:
                    resultSet = selectService.selectFromTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedSelectStatement((SelectWrapper) statement) :
                        (SelectWrapper) statement);
                    return true;
                case UPDATE:
                    updateCount = updateService.updateTable(isPreparedStatement ?
                        preparedStatementCreator.getPreparedUpdateStatement((UpdateWrapper) statement) :
                        (UpdateWrapper) statement);
                    break;
                default:
                    throw new SQLException("Statement type '" + statementType + "' is not supported.");
            }
            return false;
        }
    }

    public int getParameterCount(final String sql) throws SQLException {
        synchronized (lock) {
            final int parameterCount;
            final BaseStatement statement = parser.parse(sql);
            if (statement == null) {
                throw new IllegalStateException("The statement couldn't be created.");
            }
            final TypeOfStatement statementType = statement.getTypeOfStatement();
            switch (statementType) {
                case ALTER_TABLE:
                case CREATE_DATABASE:
                case CREATE_TABLE:
                case DROP_DATABASE:
                case DROP_TABLE:
                    parameterCount = 0;
                    break;
                case DELETE:
                    parameterCount = ((DeleteWrapper) statement).getWhereValues().size();
                    break;
                case INSERT:
                    parameterCount = ((InsertWrapper) statement).getValues().get(0).size();
                    break;
                case SELECT:
                    parameterCount = ((SelectWrapper) statement).getWhereValues().size();
                    break;
                case UPDATE:
                    parameterCount = ((UpdateWrapper) statement).getValues().size()
                        + ((UpdateWrapper) statement).getWhereValues().size();
                    break;
                default:
                    throw new SQLException("Cannot determine the type of the statement.");
            }
            return parameterCount;
        }
    }

}
