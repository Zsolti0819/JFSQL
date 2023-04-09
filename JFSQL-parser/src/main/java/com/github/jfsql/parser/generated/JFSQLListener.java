// Generated from C:/Users/destr/IdeaProjects/JFSQL/JFSQL-parser/src/main/antlr4/com/github/jfsql/parser\JFSQL.g4 by ANTLR 4.12.0
package com.github.jfsql.parser.generated;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link JFSQLParser}.
 */
public interface JFSQLListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#root}.
	 * @param ctx the parse tree
	 */
	void enterRoot(JFSQLParser.RootContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#root}.
	 * @param ctx the parse tree
	 */
	void exitRoot(JFSQLParser.RootContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(JFSQLParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(JFSQLParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#alterTable}.
	 * @param ctx the parse tree
	 */
	void enterAlterTable(JFSQLParser.AlterTableContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#alterTable}.
	 * @param ctx the parse tree
	 */
	void exitAlterTable(JFSQLParser.AlterTableContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#renameTable}.
	 * @param ctx the parse tree
	 */
	void enterRenameTable(JFSQLParser.RenameTableContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#renameTable}.
	 * @param ctx the parse tree
	 */
	void exitRenameTable(JFSQLParser.RenameTableContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#renameColumn}.
	 * @param ctx the parse tree
	 */
	void enterRenameColumn(JFSQLParser.RenameColumnContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#renameColumn}.
	 * @param ctx the parse tree
	 */
	void exitRenameColumn(JFSQLParser.RenameColumnContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#addColumn}.
	 * @param ctx the parse tree
	 */
	void enterAddColumn(JFSQLParser.AddColumnContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#addColumn}.
	 * @param ctx the parse tree
	 */
	void exitAddColumn(JFSQLParser.AddColumnContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#dropColumn}.
	 * @param ctx the parse tree
	 */
	void enterDropColumn(JFSQLParser.DropColumnContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#dropColumn}.
	 * @param ctx the parse tree
	 */
	void exitDropColumn(JFSQLParser.DropColumnContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#createTable}.
	 * @param ctx the parse tree
	 */
	void enterCreateTable(JFSQLParser.CreateTableContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#createTable}.
	 * @param ctx the parse tree
	 */
	void exitCreateTable(JFSQLParser.CreateTableContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#createDatabase}.
	 * @param ctx the parse tree
	 */
	void enterCreateDatabase(JFSQLParser.CreateDatabaseContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#createDatabase}.
	 * @param ctx the parse tree
	 */
	void exitCreateDatabase(JFSQLParser.CreateDatabaseContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#delete}.
	 * @param ctx the parse tree
	 */
	void enterDelete(JFSQLParser.DeleteContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#delete}.
	 * @param ctx the parse tree
	 */
	void exitDelete(JFSQLParser.DeleteContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#dropTable}.
	 * @param ctx the parse tree
	 */
	void enterDropTable(JFSQLParser.DropTableContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#dropTable}.
	 * @param ctx the parse tree
	 */
	void exitDropTable(JFSQLParser.DropTableContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#dropDatabase}.
	 * @param ctx the parse tree
	 */
	void enterDropDatabase(JFSQLParser.DropDatabaseContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#dropDatabase}.
	 * @param ctx the parse tree
	 */
	void exitDropDatabase(JFSQLParser.DropDatabaseContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#insert}.
	 * @param ctx the parse tree
	 */
	void enterInsert(JFSQLParser.InsertContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#insert}.
	 * @param ctx the parse tree
	 */
	void exitInsert(JFSQLParser.InsertContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#select}.
	 * @param ctx the parse tree
	 */
	void enterSelect(JFSQLParser.SelectContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#select}.
	 * @param ctx the parse tree
	 */
	void exitSelect(JFSQLParser.SelectContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#limit}.
	 * @param ctx the parse tree
	 */
	void enterLimit(JFSQLParser.LimitContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#limit}.
	 * @param ctx the parse tree
	 */
	void exitLimit(JFSQLParser.LimitContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#offset}.
	 * @param ctx the parse tree
	 */
	void enterOffset(JFSQLParser.OffsetContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#offset}.
	 * @param ctx the parse tree
	 */
	void exitOffset(JFSQLParser.OffsetContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#numericValue}.
	 * @param ctx the parse tree
	 */
	void enterNumericValue(JFSQLParser.NumericValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#numericValue}.
	 * @param ctx the parse tree
	 */
	void exitNumericValue(JFSQLParser.NumericValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#joinOperation}.
	 * @param ctx the parse tree
	 */
	void enterJoinOperation(JFSQLParser.JoinOperationContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#joinOperation}.
	 * @param ctx the parse tree
	 */
	void exitJoinOperation(JFSQLParser.JoinOperationContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#innerJoin}.
	 * @param ctx the parse tree
	 */
	void enterInnerJoin(JFSQLParser.InnerJoinContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#innerJoin}.
	 * @param ctx the parse tree
	 */
	void exitInnerJoin(JFSQLParser.InnerJoinContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#leftJoin}.
	 * @param ctx the parse tree
	 */
	void enterLeftJoin(JFSQLParser.LeftJoinContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#leftJoin}.
	 * @param ctx the parse tree
	 */
	void exitLeftJoin(JFSQLParser.LeftJoinContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#update}.
	 * @param ctx the parse tree
	 */
	void enterUpdate(JFSQLParser.UpdateContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#update}.
	 * @param ctx the parse tree
	 */
	void exitUpdate(JFSQLParser.UpdateContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#columnDefinition}.
	 * @param ctx the parse tree
	 */
	void enterColumnDefinition(JFSQLParser.ColumnDefinitionContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#columnDefinition}.
	 * @param ctx the parse tree
	 */
	void exitColumnDefinition(JFSQLParser.ColumnDefinitionContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#notNull}.
	 * @param ctx the parse tree
	 */
	void enterNotNull(JFSQLParser.NotNullContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#notNull}.
	 * @param ctx the parse tree
	 */
	void exitNotNull(JFSQLParser.NotNullContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#ifExists}.
	 * @param ctx the parse tree
	 */
	void enterIfExists(JFSQLParser.IfExistsContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#ifExists}.
	 * @param ctx the parse tree
	 */
	void exitIfExists(JFSQLParser.IfExistsContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#ifNotExists}.
	 * @param ctx the parse tree
	 */
	void enterIfNotExists(JFSQLParser.IfNotExistsContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#ifNotExists}.
	 * @param ctx the parse tree
	 */
	void exitIfNotExists(JFSQLParser.IfNotExistsContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#columnType}.
	 * @param ctx the parse tree
	 */
	void enterColumnType(JFSQLParser.ColumnTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#columnType}.
	 * @param ctx the parse tree
	 */
	void exitColumnType(JFSQLParser.ColumnTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(JFSQLParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(JFSQLParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#binaryOperator}.
	 * @param ctx the parse tree
	 */
	void enterBinaryOperator(JFSQLParser.BinaryOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#binaryOperator}.
	 * @param ctx the parse tree
	 */
	void exitBinaryOperator(JFSQLParser.BinaryOperatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#symbol}.
	 * @param ctx the parse tree
	 */
	void enterSymbol(JFSQLParser.SymbolContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#symbol}.
	 * @param ctx the parse tree
	 */
	void exitSymbol(JFSQLParser.SymbolContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#value}.
	 * @param ctx the parse tree
	 */
	void enterValue(JFSQLParser.ValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#value}.
	 * @param ctx the parse tree
	 */
	void exitValue(JFSQLParser.ValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#valuesInParentheses}.
	 * @param ctx the parse tree
	 */
	void enterValuesInParentheses(JFSQLParser.ValuesInParenthesesContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#valuesInParentheses}.
	 * @param ctx the parse tree
	 */
	void exitValuesInParentheses(JFSQLParser.ValuesInParenthesesContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#databaseURL}.
	 * @param ctx the parse tree
	 */
	void enterDatabaseURL(JFSQLParser.DatabaseURLContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#databaseURL}.
	 * @param ctx the parse tree
	 */
	void exitDatabaseURL(JFSQLParser.DatabaseURLContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#tableName}.
	 * @param ctx the parse tree
	 */
	void enterTableName(JFSQLParser.TableNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#tableName}.
	 * @param ctx the parse tree
	 */
	void exitTableName(JFSQLParser.TableNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#tableDotColumnName}.
	 * @param ctx the parse tree
	 */
	void enterTableDotColumnName(JFSQLParser.TableDotColumnNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#tableDotColumnName}.
	 * @param ctx the parse tree
	 */
	void exitTableDotColumnName(JFSQLParser.TableDotColumnNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JFSQLParser#columnName}.
	 * @param ctx the parse tree
	 */
	void enterColumnName(JFSQLParser.ColumnNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JFSQLParser#columnName}.
	 * @param ctx the parse tree
	 */
	void exitColumnName(JFSQLParser.ColumnNameContext ctx);
}