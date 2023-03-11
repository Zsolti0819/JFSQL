package com.github.jfsql.parser.core;

import com.github.jfsql.parser.dto.*;
import com.github.jfsql.parser.exception.ThrowingErrorListener;
import com.github.jfsql.parser.generated.JFSQLBaseVisitor;
import com.github.jfsql.parser.generated.JFSQLLexer;
import com.github.jfsql.parser.generated.JFSQLParser;
import com.github.jfsql.parser.generated.JFSQLVisitor;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.*;

public class Parser extends JFSQLBaseVisitor<BaseStatement> implements JFSQLVisitor<BaseStatement> {

    private static final Logger logger = LogManager.getLogger(Parser.class);

    private AlterTableStatement getAlterTableStatement(final JFSQLParser.AlterTableContext alterTableContext) {
        final String tableName = alterTableContext.tableName().getText();
        String newTableName = null;
        String oldColumnName = null;
        String newColumnName = null;
        String columnNameToAdd = null;
        String columnTypeToAdd = null;
        Boolean columnToAddCannotBeNull = null;
        String columnToDrop = null;
        if (alterTableContext.renameTable() != null) {
            newTableName = alterTableContext.renameTable().tableName().getText();
        } else if (alterTableContext.renameColumn() != null) {
            oldColumnName = alterTableContext.renameColumn().columnName().get(0).getText();
            newColumnName = alterTableContext.renameColumn().columnName().get(1).getText();
        } else if (alterTableContext.addColumn() != null) {
            columnNameToAdd = alterTableContext.addColumn().columnDefinition().columnName().getText();
            columnTypeToAdd = alterTableContext.addColumn().columnDefinition().columnType().getText();
            if (alterTableContext.addColumn().columnDefinition().notNull() != null) {
                columnToAddCannotBeNull = true;
            } else if (alterTableContext.addColumn().columnDefinition().notNull() == null) {
                columnToAddCannotBeNull = false;
            }
        } else if (alterTableContext.dropColumn() != null) {
            columnToDrop = alterTableContext.dropColumn().columnName().getText();
        }
        final AlterTableStatement alterTableStatement = new AlterTableStatement(tableName, newTableName, oldColumnName, newColumnName, columnNameToAdd,
                columnTypeToAdd, columnToAddCannotBeNull, columnToDrop);
        logger.debug(alterTableStatement);
        return alterTableStatement;
    }

    private CreateTableStatement getCreateTableStatement(final JFSQLParser.CreateTableContext createTableContext) {
        final String tableName = createTableContext.tableName().getText();
        final List<String> columns = new ArrayList<>();
        final List<String> types = new ArrayList<>();
        final Map<String, Boolean> notNullColumns = new LinkedHashMap<>();
        for (int i = 0; i < createTableContext.columnDefinition().size(); i++) {
            final String columnName = createTableContext.columnDefinition(i).columnName().getText();
            final String columnType = createTableContext.columnDefinition(i).columnType().getText();
            columns.add(columnName);
            types.add(columnType);
            if (createTableContext.columnDefinition(i).notNull() != null) {
                notNullColumns.put(columnName, true);
            } else if (createTableContext.columnDefinition(i).notNull() == null) {
                notNullColumns.put(columnName, false);
            }
        }
        final boolean ifNotExistsPresent = createTableContext.ifNotExists() != null;
        final CreateTableStatement createTableStatement = new CreateTableStatement(tableName, Collections.unmodifiableList(columns),
                Collections.unmodifiableList(types), notNullColumns, ifNotExistsPresent);
        logger.debug(createTableStatement);
        return createTableStatement;
    }

    private CreateDatabaseStatement getCreateDatabaseStatement(
            final JFSQLParser.CreateDatabaseContext createDatabaseContext) {
        final String databaseUrl = createDatabaseContext.databaseUrl().getText();
        String modifiedUrl = databaseUrl.replaceAll("[\\[\\]]", "");
        if (!modifiedUrl.endsWith(File.pathSeparator)) {
            modifiedUrl += File.separator;
        }
        final CreateDatabaseStatement createDatabaseStatement = new CreateDatabaseStatement(modifiedUrl);
        logger.debug(createDatabaseStatement);
        return createDatabaseStatement;
    }

    private DeleteStatement getDeleteStatement(final JFSQLParser.DeleteContext deleteContext) {
        final String tableName = deleteContext.tableName().getText();
        final Map<String, List<String>> whereClause = extractWhereClause(deleteContext.expr());
        final DeleteStatement deleteStatement = new DeleteStatement(tableName, whereClause.get("whereColumns"), whereClause.get("whereValues"),
                whereClause.get("symbols"), whereClause.get("binaryOperators"));
        logger.debug(deleteStatement);
        return deleteStatement;
    }

    private DropDatabaseStatement getDropDatabaseStatement(final JFSQLParser.DropDatabaseContext dropDatabaseContext) {
        final String databaseUrl = dropDatabaseContext.databaseUrl().getText();
        String modifiedUrl = databaseUrl.replaceAll("[\\[\\]]", "");
        if (!modifiedUrl.endsWith(File.pathSeparator)) {
            modifiedUrl += File.separator;
        }
        final DropDatabaseStatement dropDatabaseStatement = new DropDatabaseStatement(modifiedUrl);
        logger.debug(dropDatabaseStatement);
        return dropDatabaseStatement;
    }

    private DropTableStatement getDropTableStatement(final JFSQLParser.DropTableContext dropTableContext) {
        final String tableName = dropTableContext.tableName().getText();
        final boolean ifExistsPresent = dropTableContext.ifExists() != null;
        final DropTableStatement dropTableStatement = new DropTableStatement(tableName, ifExistsPresent);
        logger.debug(dropTableStatement);
        return dropTableStatement;
    }

    private InsertStatement getInsertStatement(final JFSQLParser.InsertContext insertContext) {
        final String tableName = insertContext.tableName().getText();
        final List<String> columns = new ArrayList<>();

        for (int i = 0; i < insertContext.columnName().size(); i++) {
            columns.add(insertContext.columnName(i).getText());
        }

        final List<List<String>> listOfValueLists = new ArrayList<>();
        for (int i = 0; i < insertContext.valuesInParentheses().size(); i++) {
            final List<String> valueList = new ArrayList<>();
            for (int j = 0; j < insertContext.valuesInParentheses(i).value().size(); j++) {
                String value = insertContext.valuesInParentheses(i).value(j).getText();
                if (value.startsWith("'") && value.endsWith("'")) {
                    value = value.substring(1, value.length() - 1);
                }
                valueList.add(value);
            }
            listOfValueLists.add(Collections.unmodifiableList(valueList));
        }
        final InsertStatement insertStatement = new InsertStatement(tableName, Collections.unmodifiableList(columns),
                Collections.unmodifiableList(listOfValueLists));
        logger.debug(insertStatement);
        return insertStatement;
    }

    private SelectStatement getSelectStatement(final JFSQLParser.SelectContext selectContext) {
        final String selectTable = selectContext.tableName().getText();

        final List<String> selectColumns = new ArrayList<>();
        selectContext.columnName().forEach(columnNameContext -> selectColumns.add(columnNameContext.getText()));

        List<String> joinTables = null;
        List<JoinType> joinTypes = null;
        final List<List<String>> listOfJoinColumnsWithPrefixes = new ArrayList<>();
        if (selectContext.joinOperation() != null) {
            joinTables = new ArrayList<>();
            joinTypes = new ArrayList<>();
            for (int i = 0; i < selectContext.joinOperation().size(); i++) {
                final List<String> joinColumnsWithPrefixes = new ArrayList<>();
                if (selectContext.joinOperation().get(i).innerJoin() != null) {
                    joinTypes.add(JoinType.INNER_JOIN);
                    joinTables.add(selectContext.joinOperation().get(i).innerJoin().tableName().getText());
                    joinColumnsWithPrefixes.add(selectContext.joinOperation().get(i).innerJoin().tableDotColumnName().get(0).getText());
                    joinColumnsWithPrefixes.add(selectContext.joinOperation().get(i).innerJoin().tableDotColumnName().get(1).getText());
                } else if (selectContext.joinOperation().get(i).leftJoin() != null) {
                    joinTypes.add(JoinType.LEFT_JOIN);
                    joinTables.add(selectContext.joinOperation().get(i).leftJoin().tableName().getText());
                    joinColumnsWithPrefixes.add(selectContext.joinOperation().get(i).leftJoin().tableDotColumnName().get(0).getText());
                    joinColumnsWithPrefixes.add(selectContext.joinOperation().get(i).leftJoin().tableDotColumnName().get(1).getText());
                }
                listOfJoinColumnsWithPrefixes.add(joinColumnsWithPrefixes);
            }
        }

        final Map<String, List<String>> whereClause = extractWhereClause(selectContext.expr());
        final SelectStatement selectStatement = new SelectStatement(selectTable, joinTables, joinTypes, Collections.unmodifiableList(selectColumns),
                Collections.unmodifiableList(listOfJoinColumnsWithPrefixes), whereClause.get("whereColumns"), whereClause.get("whereValues"),
                whereClause.get("symbols"), whereClause.get("binaryOperators"));
        logger.debug(selectStatement);
        return selectStatement;
    }

    private UpdateStatement getUpdateStatement(final JFSQLParser.UpdateContext updateContext) {
        final String tableName = updateContext.tableName().getText();
        final List<String> columns = new ArrayList<>();
        final List<String> values = new ArrayList<>();
        for (int i = 0; i < updateContext.columnName().size(); i++) {
            String value = updateContext.value(i).getText();
            if (value.startsWith("'") && value.endsWith("'")) {
                value = value.substring(1, value.length() - 1);
            }
            columns.add(updateContext.columnName(i).getText());
            values.add(value);
        }

        final Map<String, List<String>> whereClause = extractWhereClause(updateContext.expr());
        final UpdateStatement updateStatement = new UpdateStatement(tableName, Collections.unmodifiableList(columns),
                Collections.unmodifiableList(values), whereClause.get("whereColumns"), whereClause.get("whereValues"),
                whereClause.get("symbols"), whereClause.get("binaryOperators"));
        logger.debug(updateStatement);
        return updateStatement;
    }

    private Map<String, List<String>> extractWhereClause(final JFSQLParser.ExprContext expr) {
        final Map<String, List<String>> whereClause = new HashMap<>();
        final List<String> whereColumns = new ArrayList<>();
        final List<String> whereValues = new ArrayList<>();
        final List<String> symbols = new ArrayList<>();
        final List<String> binaryOperators = new ArrayList<>();
        if (expr != null) {
            for (int i = 0; i < expr.value().size(); i++) {
                String whereValue = expr.value().get(i).getText();
                if (whereValue.startsWith("'") && whereValue.endsWith("'")) {
                    whereValue = whereValue.substring(1, whereValue.length() - 1);
                }
                whereColumns.add(expr.columnName().get(i).getText());
                whereValues.add(whereValue);
                symbols.add(expr.symbol().get(i).getText());
                if (expr.binaryOperator(i) != null) {
                    binaryOperators.add(expr.binaryOperator().get(i).getText());
                }
            }
        }

        whereClause.put("whereColumns", Collections.unmodifiableList(whereColumns));
        whereClause.put("whereValues", Collections.unmodifiableList(whereValues));
        whereClause.put("symbols", Collections.unmodifiableList(symbols));
        whereClause.put("binaryOperators", Collections.unmodifiableList(binaryOperators));

        return whereClause;
    }

    @Override
    public BaseStatement visitRoot(final JFSQLParser.RootContext rootContext) {
        if (rootContext.statement().alterTable() != null) {
            return getAlterTableStatement(rootContext.statement().alterTable());
        } else if (rootContext.statement().createTable() != null) {
            return getCreateTableStatement(rootContext.statement().createTable());
        } else if (rootContext.statement().createDatabase() != null) {
            return getCreateDatabaseStatement(rootContext.statement().createDatabase());
        } else if (rootContext.statement().delete() != null) {
            return getDeleteStatement(rootContext.statement().delete());
        } else if (rootContext.statement().dropDatabase() != null) {
            return getDropDatabaseStatement(rootContext.statement().dropDatabase());
        } else if (rootContext.statement().dropTable() != null) {
            return getDropTableStatement(rootContext.statement().dropTable());
        } else if (rootContext.statement().insert() != null) {
            return getInsertStatement(rootContext.statement().insert());
        } else if (rootContext.statement().select() != null) {
            return getSelectStatement(rootContext.statement().select());
        } else if (rootContext.statement().update() != null) {
            return getUpdateStatement(rootContext.statement().update());
        }
        return null;
    }

    public BaseStatement parse(final String sql) {
        final JFSQLLexer lexer = new JFSQLLexer(CharStreams.fromString(sql));
        lexer.removeErrorListeners();
        lexer.addErrorListener(ThrowingErrorListener.INSTANCE);
        final JFSQLParser parser = new JFSQLParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(ThrowingErrorListener.INSTANCE);

        final BaseStatement statement;

        try {
            final JFSQLParser.RootContext parseContext = parser.root();
            statement = visitRoot(parseContext);
        } catch (final RecognitionException | ParseCancellationException e) {
            System.err.println("The statement '" + sql + "' is invalid. Please check the syntax.\n" + e.getMessage());
            return null;
        }

        return statement;
    }
}
