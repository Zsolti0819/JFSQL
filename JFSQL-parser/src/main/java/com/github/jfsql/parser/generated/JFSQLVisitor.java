// Generated from C:/Users/destr/IdeaProjects/JFSQL/JFSQL-parser/src/main/antlr4/com/github/jfsql/parser\JFSQL.g4 by ANTLR 4.12.0
package com.github.jfsql.parser.generated;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link JFSQLParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface JFSQLVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#root}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRoot(JFSQLParser.RootContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatement(JFSQLParser.StatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#alterTable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlterTable(JFSQLParser.AlterTableContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#renameTable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRenameTable(JFSQLParser.RenameTableContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#renameColumn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRenameColumn(JFSQLParser.RenameColumnContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#addColumn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddColumn(JFSQLParser.AddColumnContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#dropColumn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDropColumn(JFSQLParser.DropColumnContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#createTable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCreateTable(JFSQLParser.CreateTableContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#delete}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDelete(JFSQLParser.DeleteContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#dropTable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDropTable(JFSQLParser.DropTableContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#insert}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInsert(JFSQLParser.InsertContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#select}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelect(JFSQLParser.SelectContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#limit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLimit(JFSQLParser.LimitContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#offset}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOffset(JFSQLParser.OffsetContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#numericValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumericValue(JFSQLParser.NumericValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#joinOperation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJoinOperation(JFSQLParser.JoinOperationContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#innerJoin}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInnerJoin(JFSQLParser.InnerJoinContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#leftJoin}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLeftJoin(JFSQLParser.LeftJoinContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#update}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUpdate(JFSQLParser.UpdateContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#columnDefinition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitColumnDefinition(JFSQLParser.ColumnDefinitionContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#notNull}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNotNull(JFSQLParser.NotNullContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#ifExists}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfExists(JFSQLParser.IfExistsContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#ifNotExists}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfNotExists(JFSQLParser.IfNotExistsContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#columnType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitColumnType(JFSQLParser.ColumnTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(JFSQLParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#binaryOperator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinaryOperator(JFSQLParser.BinaryOperatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#symbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSymbol(JFSQLParser.SymbolContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValue(JFSQLParser.ValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#valuesInParentheses}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValuesInParentheses(JFSQLParser.ValuesInParenthesesContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#tableName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTableName(JFSQLParser.TableNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#tableDotColumnName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTableDotColumnName(JFSQLParser.TableDotColumnNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JFSQLParser#columnName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitColumnName(JFSQLParser.ColumnNameContext ctx);
}