package com.github.jfsql.driver.services;

import com.github.jfsql.driver.cache.Cache;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.DatabaseManager;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
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

    private final DatabaseManager databaseManager;
    private final TransactionManager transactionManager;
    private final Cache cache;
    private final TableFinder tableFinder;
    private final SemanticValidator semanticValidator;
    private final ColumnToTypeMapper columnToTypeMapper;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;
    private final Parser parser;

    private final AlterTableService alterTableService;
    private final CreateDatabaseService createDatabaseService;
    private final DropDatabaseService dropDatabaseService;
    private final CreateTableService createTableService;
    private final InsertService insertService;
    private final SelectService selectService;
    private final UpdateService updateService;
    private final DeleteService deleteService;
    private final DropTableService dropTableService;

    private final PreparedStatementCreator preparedStatementCreator;
    private ResultSet resultSet;
    private int updateCount = 0;
    private Object[] parameters;

    public StatementServiceManager(final DatabaseManager databaseManager, final Cache cache,
        final TransactionManager transactionManager, final Reader reader) {
        this.databaseManager = databaseManager;
        this.transactionManager = transactionManager;
        this.cache = cache;
        this.reader = reader;

        parser = new Parser();
        tableFinder = new TableFinder(databaseManager);
        semanticValidator = new SemanticValidator();
        columnToTypeMapper = new ColumnToTypeMapper();
        whereConditionSolver = new WhereConditionSolver();
        preparedStatementCreator = new PreparedStatementCreator(tableFinder, this);
        alterTableService = new AlterTableService(tableFinder, databaseManager, transactionManager,
            semanticValidator, reader);
        createDatabaseService = new CreateDatabaseService(databaseManager, semanticValidator, reader);
        dropDatabaseService = new DropDatabaseService(databaseManager, semanticValidator, reader);
        createTableService = new CreateTableService(databaseManager, transactionManager, semanticValidator, reader);
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

    public ResultSet executeQuery(final String sql) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        if (!(TypeOfStatement.SELECT.equals(statement.getTypeOfStatement()))) {
            throw new SQLException("Cannot execute executeQuery() because statement was not a Select statement.");
        }
        return selectService.selectFromTable((SelectWrapper) statement);
    }

    private void executeQuery(final BaseStatement statement) throws SQLException {
        resultSet = selectService.selectFromTable((SelectWrapper) statement);
    }

    public int executeUpdate(final String arg0) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(arg0);
        switch (statement.getTypeOfStatement()) {
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
                updateCount = deleteService.deleteFromTable((DeleteWrapper) statement);
                break;
            case DROP_DATABASE:
                updateCount = dropDatabaseService.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                updateCount = dropTableService.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                updateCount = insertService.insertIntoTable((InsertWrapper) statement);
                break;
            case SELECT:
                resultSet = selectService.selectFromTable((SelectWrapper) statement);
                break;
            case UPDATE:
                updateCount = updateService.updateTable((UpdateWrapper) statement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
        return updateCount;
    }

    private void executeUpdate(final BaseStatement statement) throws SQLException {
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
                alterTableService.alterTable((AlterTableWrapper) statement);
                break;
            case CREATE_DATABASE:
                createDatabaseService.createDatabase((CreateDatabaseWrapper) statement);
                break;
            case CREATE_TABLE:
                createTableService.createTable((CreateTableWrapper) statement);
                break;
            case DELETE:
                deleteService.deleteFromTable((DeleteWrapper) statement);
                break;
            case DROP_DATABASE:
                dropDatabaseService.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                dropTableService.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                insertService.insertIntoTable((InsertWrapper) statement);
                break;
            case UPDATE:
                updateService.updateTable((UpdateWrapper) statement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
    }

    public boolean execute(final String sql) throws SQLException {
        final BaseStatement statement = getFromCacheOrParseStatement(sql);
        if (TypeOfStatement.SELECT.equals(statement.getTypeOfStatement())) {
            executeQuery(statement);
            return true;
        } else {
            executeUpdate(statement);
            return false;
        }
    }

    // Prepared Statements

    public ResultSet executeQueryPreparedStatement(final String sql) throws SQLException {
        BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        if (!(TypeOfStatement.SELECT.equals(statement.getTypeOfStatement()))) {
            throw new SQLException("Cannot execute executeQuery() because statement was not a Select statement.");
        }
        statement = preparedStatementCreator.getPreparedSelectStatement((SelectWrapper) statement);
        resultSet = selectService.selectFromTable((SelectWrapper) statement);
        return resultSet;
    }

    private void executeQueryPreparedStatement(final BaseStatement statement) throws SQLException {
        final SelectWrapper preparedSelectStatement = preparedStatementCreator.getPreparedSelectStatement(
            (SelectWrapper) statement);
        resultSet = selectService.selectFromTable(preparedSelectStatement);
    }

    public int executeUpdatePreparedStatement(final String sql) throws SQLException {
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        switch (statement.getTypeOfStatement()) {
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
                final DeleteWrapper preparedDeleteStatement = preparedStatementCreator.getPreparedDeleteStatement(
                    (DeleteWrapper) statement);
                updateCount = deleteService.deleteFromTable(preparedDeleteStatement);
                break;
            case DROP_DATABASE:
                updateCount = dropDatabaseService.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                updateCount = dropTableService.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                final InsertWrapper preparedInsertStatement = preparedStatementCreator.getPreparedInsertStatement(
                    (InsertWrapper) statement);
                updateCount = insertService.insertIntoTable(preparedInsertStatement);
                break;
            case UPDATE:
                final UpdateWrapper preparedUpdateStatement = preparedStatementCreator.getPreparedUpdateStatement(
                    (UpdateWrapper) statement);
                updateCount = updateService.updateTable(preparedUpdateStatement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
        return updateCount;
    }

    private void executeUpdatePreparedStatement(final BaseStatement statement) throws SQLException {
        switch (statement.getTypeOfStatement()) {
            case ALTER_TABLE:
                alterTableService.alterTable((AlterTableWrapper) statement);
                break;
            case CREATE_DATABASE:
                createDatabaseService.createDatabase((CreateDatabaseWrapper) statement);
                break;
            case CREATE_TABLE:
                createTableService.createTable((CreateTableWrapper) statement);
                break;
            case DELETE:
                final DeleteWrapper preparedDeleteStatement = preparedStatementCreator.getPreparedDeleteStatement(
                    (DeleteWrapper) statement);
                deleteService.deleteFromTable(preparedDeleteStatement);
                break;
            case DROP_DATABASE:
                dropDatabaseService.dropDatabase((DropDatabaseWrapper) statement);
                break;
            case DROP_TABLE:
                dropTableService.dropTable((DropTableWrapper) statement);
                break;
            case INSERT:
                final InsertWrapper preparedInsertStatement = preparedStatementCreator.getPreparedInsertStatement(
                    (InsertWrapper) statement);
                insertService.insertIntoTable(preparedInsertStatement);
                break;
            case UPDATE:
                final UpdateWrapper preparedUpdateStatement = preparedStatementCreator.getPreparedUpdateStatement(
                    (UpdateWrapper) statement);
                updateService.updateTable(preparedUpdateStatement);
                break;
            default:
                throw new SQLException("This statement type is not supported.");
        }
    }

    public boolean executePreparedStatement(final String sql) throws SQLException {
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        if (TypeOfStatement.SELECT.equals(statement.getTypeOfStatement())) {
            final SelectWrapper preparedSelectStatement = preparedStatementCreator.getPreparedSelectStatement(
                (SelectWrapper) statement);
            executeQueryPreparedStatement(preparedSelectStatement);
            return true;
        } else {
            executeUpdatePreparedStatement(statement);
            return false;
        }
    }

    public int getParameterCount(final String sql) throws SQLException {
        final int parameterCount;
        final BaseStatement statement = parser.parse(sql);
        if (statement == null) {
            throw new IllegalStateException("The statement couldn't be created.");
        }
        switch (statement.getTypeOfStatement()) {
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
