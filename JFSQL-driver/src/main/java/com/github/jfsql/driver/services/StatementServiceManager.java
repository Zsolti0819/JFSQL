package com.github.jfsql.driver.services;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.transactions.TransactionManager;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.TableFinder;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.AlterTableWrapper;
import com.github.jfsql.parser.dto.CreateDatabaseWrapper;
import com.github.jfsql.parser.dto.CreateTableWrapper;
import com.github.jfsql.parser.dto.DeleteWrapper;
import com.github.jfsql.parser.dto.DropDatabaseWrapper;
import com.github.jfsql.parser.dto.DropTableWrapper;
import com.github.jfsql.parser.dto.InsertWrapper;
import com.github.jfsql.parser.dto.SelectWrapper;
import com.github.jfsql.parser.dto.UpdateWrapper;
import java.sql.ResultSet;
import java.sql.SQLException;
import lombok.Data;

@Data
public class StatementServiceManager {

    private final TransactionManager transactionManager;
    private final TableFinder tableFinder;
    private final SemanticValidator semanticValidator;
    private final ColumnToTypeMapper columnToTypeMapper;
    private final WhereConditionSolver whereConditionSolver;
    private final Reader reader;
    private final Database database;

    public StatementServiceManager(final Database database, final TableFinder tableFinder,
        final TransactionManager transactionManager, final Reader reader) {
        this.database = database;
        this.tableFinder = tableFinder;
        this.transactionManager = transactionManager;
        this.reader = reader;
        semanticValidator = new SemanticValidator();
        columnToTypeMapper = new ColumnToTypeMapper();
        whereConditionSolver = new WhereConditionSolver();
    }

    public void alterTable(final AlterTableWrapper statement) throws SQLException {
        new AlterTableService(tableFinder, database, transactionManager, semanticValidator, reader).alterTable(
            statement);
    }

    public void createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        new CreateDatabaseService(transactionManager, semanticValidator, reader).createDatabase(statement);
    }

    public int dropDatabase(final DropDatabaseWrapper statement) throws SQLException {
        return new DropDatabaseService(transactionManager, semanticValidator, reader).dropDatabase(statement);
    }

    public void createTable(final CreateTableWrapper statement) throws SQLException {
        new CreateTableService(database, transactionManager, semanticValidator, reader).createTable(statement);
    }

    public int insertIntoTable(final InsertWrapper statement) throws SQLException {
        return new InsertService(tableFinder, transactionManager, semanticValidator, reader).insertIntoTable(statement);
    }

    public ResultSet selectFromTable(final SelectWrapper statement) throws SQLException {
        return new SelectService(tableFinder, semanticValidator, columnToTypeMapper, whereConditionSolver,
            reader).selectFromTable(statement);
    }

    public int updateTable(final UpdateWrapper statement) throws SQLException {
        return new UpdateService(tableFinder, transactionManager, semanticValidator, columnToTypeMapper,
            whereConditionSolver, reader).updateTable(
            statement);
    }

    public int deleteFromTable(final DeleteWrapper statement) throws SQLException {
        return new DeleteService(tableFinder, transactionManager, semanticValidator, whereConditionSolver,
            reader).deleteFromTable(statement);
    }

    public int dropTable(final DropTableWrapper statement) throws SQLException {
        return new DropTableService(tableFinder, database, transactionManager, semanticValidator, reader).dropTable(
            statement);
    }

}
