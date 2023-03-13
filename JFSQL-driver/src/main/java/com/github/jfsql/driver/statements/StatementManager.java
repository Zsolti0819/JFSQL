package com.github.jfsql.driver.statements;

import com.github.jfsql.driver.dto.Database;
import com.github.jfsql.driver.dto.Table;
import com.github.jfsql.driver.persistence.Reader;
import com.github.jfsql.driver.persistence.Writer;
import com.github.jfsql.driver.transactions.Transaction;
import com.github.jfsql.driver.util.ColumnToTypeMapper;
import com.github.jfsql.driver.util.WhereConditionSolver;
import com.github.jfsql.driver.validation.SemanticValidator;
import com.github.jfsql.parser.dto.*;
import lombok.Data;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Objects;

@Data
public class StatementManager {

    private final Transaction transaction;
    private final SemanticValidator semanticValidator;
    private final ColumnToTypeMapper columnToTypeMapper;
    private final WhereConditionSolver whereConditionSolver;
    private final TableFinder tableFinder;
    private final Reader reader;
    private final Writer writer;
    private final Database database;

    public StatementManager(final Database database, final Transaction transaction, final Reader reader, final Writer writer) {
        this.transaction = transaction;
        this.database = database;
        this.reader = reader;
        this.writer = writer;
        tableFinder = new TableFinder(database);
        semanticValidator = new SemanticValidator();
        columnToTypeMapper = new ColumnToTypeMapper();
        whereConditionSolver = new WhereConditionSolver();
    }

    public void alterTable(final AlterTableWrapper statement) throws SQLException {
        new AlterTableService(tableFinder, database, transaction, semanticValidator, reader, writer).alterTable(statement);
    }

    public void createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        new CreateDatabaseService(transaction, semanticValidator, writer).createDatabase(statement);
    }

    public int dropDatabase(final DropDatabaseWrapper statement) throws SQLException {
        return new DropDatabaseService(transaction, semanticValidator, writer).dropDatabase(statement);
    }

    public void createTable(final CreateTableWrapper statement) throws SQLException {
        new CreateTableService(database, transaction, semanticValidator, writer).createTable(statement);
    }

    public int insertIntoTable(final InsertWrapper statement) throws SQLException {
        return new InsertService(tableFinder, transaction, semanticValidator, reader).insertIntoTable(statement);
    }

    public ResultSet selectFromTable(final SelectWrapper statement) throws SQLException {
        return new SelectService(tableFinder, semanticValidator, columnToTypeMapper, whereConditionSolver, reader).selectFromTable(statement);
    }

    public int updateTable(final UpdateWrapper statement) throws SQLException {
        return new UpdateService(tableFinder, transaction, semanticValidator, columnToTypeMapper, whereConditionSolver, reader).updateTable(
                statement);
    }

    public int deleteFromTable(final DeleteWrapper statement) throws SQLException {
        return new DeleteService(tableFinder, transaction, semanticValidator, whereConditionSolver, reader).deleteFromTable(statement);
    }

    public int dropTable(final DropTableWrapper statement) throws SQLException {
        return new DropTableService(this, transaction, semanticValidator, reader).dropTable(statement);
    }

    // Common methods

    public Table getTableByName(final String tableName) throws SQLException {
        return database.getTables().stream()
                .filter(t -> Objects.equals(tableName, t.getName()))
                .findFirst()
                .orElseThrow(() -> new SQLException("\"" + tableName + "\" not found"));
    }

}
