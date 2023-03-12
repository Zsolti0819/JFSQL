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
    private final Reader reader;
    private final Writer writer;
    private Database database;

    public StatementManager(final Transaction transaction, final Reader reader, final Writer writer) {
        this.transaction = transaction;
        database = transaction.getDatabase();
        this.reader = reader;
        this.writer = writer;
        semanticValidator = new SemanticValidator();
        columnToTypeMapper = new ColumnToTypeMapper();
        whereConditionSolver = new WhereConditionSolver();
    }

    public void alterTable(final AlterTableWrapper statement) throws SQLException {
        new AlterTableService(this, transaction, semanticValidator, reader, writer).alterTable(statement);
    }

    public void createDatabase(final CreateDatabaseWrapper statement) throws SQLException {
        new CreateDatabaseService(this, semanticValidator, writer).createDatabase(statement);
    }

    public int dropDatabase(final DropDatabaseWrapper statement) throws SQLException {
        return new DropDatabaseService(this, writer, semanticValidator).dropDatabase(statement);
    }

    public void createTable(final CreateTableWrapper statement) throws SQLException {
        new CreateTableService(this, semanticValidator, writer).createTable(statement);
    }

    public int insertIntoTable(final InsertWrapper statement) throws SQLException {
        return new InsertService(this, semanticValidator, reader).insertIntoTable(statement);
    }

    public ResultSet selectFromTable(final SelectWrapper statement) throws SQLException {
        return new SelectService(this, semanticValidator, columnToTypeMapper, whereConditionSolver, reader).selectFromTable(statement);
    }

    public int updateTable(final UpdateWrapper statement) throws SQLException {
        return new UpdateService(this, semanticValidator, columnToTypeMapper, whereConditionSolver, reader).updateTable(
                statement);
    }

    public int deleteFromTable(final DeleteWrapper statement) throws SQLException {
        return new DeleteService(this, semanticValidator, whereConditionSolver, reader).deleteFromTable(statement);
    }

    public int dropTable(final DropTableWrapper statement) throws SQLException {
        return new DropTableService(this, semanticValidator, reader).dropTable(statement);
    }

    // Common methods

    public Table getTableByName(final String tableName) throws SQLException {
        return database.getTables().stream()
                .filter(t -> Objects.equals(tableName, t.getName()))
                .findFirst()
                .orElseThrow(() -> new SQLException("\"" + tableName + "\" not found"));
    }

    public void executeDMLOperation(final Table table) throws SQLException {
        if (!transaction.getAutoCommit()) {
            writer.addTableToUncommittedObjects(table);
        } else if (transaction.getAutoCommit()) {
            try {
                writer.writeTable(table);
                transaction.commit();
            } catch (final SQLException e) {
                e.printStackTrace();
                transaction.rollback();
            }
        }
    }

    public void executeDDLOperation(final Table table) throws SQLException {
        if (!transaction.getAutoCommit()) {
            writer.addSchemaToUncommittedObjects(table);
            writer.addTableToUncommittedObjects(table);
            writer.addDatabaseToUncommittedObjects(database);
        } else if (transaction.getAutoCommit()) {
            try {
                writer.writeSchema(table);
                writer.writeTable(table);
                writer.writeDatabaseFile(database);
                transaction.commit();
            } catch (final SQLException e) {
                e.printStackTrace();
                transaction.rollback();
            }
        }
    }

    public void executeDropTableOperation() throws SQLException {
        if (!transaction.getAutoCommit()) {
            writer.addDatabaseToUncommittedObjects(database);
        } else if (transaction.getAutoCommit()) {
            try {
                writer.writeDatabaseFile(database);
                transaction.commit();
            } catch (final SQLException e) {
                e.printStackTrace();
                transaction.rollback();
            }
        }
    }

    public void executeCreateDatabaseOperation(final Database database) throws SQLException {
        transaction.initDatabase(database);
    }
}
