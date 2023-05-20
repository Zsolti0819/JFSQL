// Generated from C:/Users/destr/IdeaProjects/JFSQL/JFSQL-parser/src/main/antlr4/com/github/jfsql/parser\JFSQL.g4 by ANTLR 4.12.0
package com.github.jfsql.parser.generated;

import java.util.List;
import org.antlr.v4.runtime.NoViableAltException;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.RuntimeMetaData;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.Vocabulary;
import org.antlr.v4.runtime.VocabularyImpl;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.TerminalNode;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class JFSQLParser extends Parser {

    static {
        RuntimeMetaData.checkVersion("4.12.0", RuntimeMetaData.VERSION);
    }

    protected static final DFA[] _decisionToDFA;
    protected static final PredictionContextCache _sharedContextCache =
        new PredictionContextCache();
    public static final int
        T__0 = 1, COL = 2, SCOL = 3, DOT = 4, OPEN_PAR = 5, CLOSE_PAR = 6, LT = 7, LT_EQ = 8,
        GT = 9, GT_EQ = 10, EQ = 11, NOT_EQ = 12, QUESTION_MARK = 13, NOT = 14, NULL = 15, IF = 16,
        EXISTS = 17, LIKE = 18, LIMIT = 19, OFFSET = 20, ORDER = 21, BY = 22, ASC = 23, DESC = 24,
        AND = 25, OR = 26, ADD = 27, ALTER = 28, CREATE = 29, COLUMN = 30, DELETE = 31, DATABASE = 32,
        DROP = 33, FROM = 34, INSERT = 35, INTO = 36, SELECT = 37, SET = 38, TABLE = 39, UPDATE = 40,
        VALUES = 41, WHERE = 42, RENAME = 43, TO = 44, ON = 45, LEFT = 46, JOIN = 47, INNER = 48,
        OUTER = 49, INTEGER = 50, REAL = 51, TEXT = 52, BLOB = 53, IDENTIFIER = 54, NUMERIC_LITERAL = 55,
        STRING_LITERAL = 56, SPACES = 57;
    public static final int
        RULE_root = 0, RULE_statement = 1, RULE_alterTable = 2, RULE_renameTable = 3,
        RULE_renameColumn = 4, RULE_addColumn = 5, RULE_dropColumn = 6, RULE_createTable = 7,
        RULE_delete = 8, RULE_dropTable = 9, RULE_insert = 10, RULE_select = 11,
        RULE_orderBy = 12, RULE_ordering = 13, RULE_limit = 14, RULE_offset = 15,
        RULE_numericValue = 16, RULE_joinOperation = 17, RULE_innerJoin = 18,
        RULE_leftJoin = 19, RULE_update = 20, RULE_columnDefinition = 21, RULE_notNull = 22,
        RULE_ifExists = 23, RULE_ifNotExists = 24, RULE_columnType = 25, RULE_expr = 26,
        RULE_binaryOperator = 27, RULE_symbol = 28, RULE_value = 29, RULE_valuesInParentheses = 30,
        RULE_tableName = 31, RULE_tableDotColumnName = 32, RULE_columnName = 33;

    private static String[] makeRuleNames() {
        return new String[]{
            "root", "statement", "alterTable", "renameTable", "renameColumn", "addColumn",
            "dropColumn", "createTable", "delete", "dropTable", "insert", "select",
            "orderBy", "ordering", "limit", "offset", "numericValue", "joinOperation",
            "innerJoin", "leftJoin", "update", "columnDefinition", "notNull", "ifExists",
            "ifNotExists", "columnType", "expr", "binaryOperator", "symbol", "value",
            "valuesInParentheses", "tableName", "tableDotColumnName", "columnName"
        };
    }

    public static final String[] ruleNames = makeRuleNames();

    private static String[] makeLiteralNames() {
        return new String[]{
            null, "'default'", "','", "';'", "'.'", "'('", "')'", "'<'", "'<='",
            "'>'", "'>='", "'='", "'!='", "'?'"
        };
    }

    private static final String[] _LITERAL_NAMES = makeLiteralNames();

    private static String[] makeSymbolicNames() {
        return new String[]{
            null, null, "COL", "SCOL", "DOT", "OPEN_PAR", "CLOSE_PAR", "LT", "LT_EQ",
            "GT", "GT_EQ", "EQ", "NOT_EQ", "QUESTION_MARK", "NOT", "NULL", "IF",
            "EXISTS", "LIKE", "LIMIT", "OFFSET", "ORDER", "BY", "ASC", "DESC", "AND",
            "OR", "ADD", "ALTER", "CREATE", "COLUMN", "DELETE", "DATABASE", "DROP",
            "FROM", "INSERT", "INTO", "SELECT", "SET", "TABLE", "UPDATE", "VALUES",
            "WHERE", "RENAME", "TO", "ON", "LEFT", "JOIN", "INNER", "OUTER", "INTEGER",
            "REAL", "TEXT", "BLOB", "IDENTIFIER", "NUMERIC_LITERAL", "STRING_LITERAL",
            "SPACES"
        };
    }

    private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
    public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

    /**
     * @deprecated Use {@link #VOCABULARY} instead.
     */
    @Deprecated
    public static final String[] tokenNames;

    static {
        tokenNames = new String[_SYMBOLIC_NAMES.length];
        for (int i = 0; i < tokenNames.length; i++) {
            tokenNames[i] = VOCABULARY.getLiteralName(i);
            if (tokenNames[i] == null) {
                tokenNames[i] = VOCABULARY.getSymbolicName(i);
            }

            if (tokenNames[i] == null) {
                tokenNames[i] = "<INVALID>";
            }
        }
    }

    @Override
    @Deprecated
    public String[] getTokenNames() {
        return tokenNames;
    }

    @Override

    public Vocabulary getVocabulary() {
        return VOCABULARY;
    }

    @Override
    public String getGrammarFileName() {
        return "JFSQL.g4";
    }

    @Override
    public String[] getRuleNames() {
        return ruleNames;
    }

    @Override
    public String getSerializedATN() {
        return _serializedATN;
    }

    @Override
    public ATN getATN() {
        return _ATN;
    }

    public JFSQLParser(TokenStream input) {
        super(input);
        _interp = new ParserATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
    }

    @SuppressWarnings("CheckReturnValue")
    public static class RootContext extends ParserRuleContext {

        public StatementContext statement() {
            return getRuleContext(StatementContext.class, 0);
        }

        public TerminalNode EOF() {
            return getToken(JFSQLParser.EOF, 0);
        }

        public TerminalNode SCOL() {
            return getToken(JFSQLParser.SCOL, 0);
        }

        public RootContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_root;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterRoot(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitRoot(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitRoot(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final RootContext root() throws RecognitionException {
        RootContext _localctx = new RootContext(_ctx, getState());
        enterRule(_localctx, 0, RULE_root);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(68);
                statement();
                setState(70);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == SCOL) {
                    {
                        setState(69);
                        match(SCOL);
                    }
                }

                setState(72);
                match(EOF);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class StatementContext extends ParserRuleContext {

        public AlterTableContext alterTable() {
            return getRuleContext(AlterTableContext.class, 0);
        }

        public CreateTableContext createTable() {
            return getRuleContext(CreateTableContext.class, 0);
        }

        public DeleteContext delete() {
            return getRuleContext(DeleteContext.class, 0);
        }

        public DropTableContext dropTable() {
            return getRuleContext(DropTableContext.class, 0);
        }

        public InsertContext insert() {
            return getRuleContext(InsertContext.class, 0);
        }

        public SelectContext select() {
            return getRuleContext(SelectContext.class, 0);
        }

        public UpdateContext update() {
            return getRuleContext(UpdateContext.class, 0);
        }

        public StatementContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_statement;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterStatement(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitStatement(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitStatement(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final StatementContext statement() throws RecognitionException {
        StatementContext _localctx = new StatementContext(_ctx, getState());
        enterRule(_localctx, 2, RULE_statement);
        try {
            setState(81);
            _errHandler.sync(this);
            switch (_input.LA(1)) {
                case ALTER:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(74);
                    alterTable();
                }
                break;
                case CREATE:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(75);
                    createTable();
                }
                break;
                case DELETE:
                    enterOuterAlt(_localctx, 3);
                {
                    setState(76);
                    delete();
                }
                break;
                case DROP:
                    enterOuterAlt(_localctx, 4);
                {
                    setState(77);
                    dropTable();
                }
                break;
                case INSERT:
                    enterOuterAlt(_localctx, 5);
                {
                    setState(78);
                    insert();
                }
                break;
                case SELECT:
                    enterOuterAlt(_localctx, 6);
                {
                    setState(79);
                    select();
                }
                break;
                case UPDATE:
                    enterOuterAlt(_localctx, 7);
                {
                    setState(80);
                    update();
                }
                break;
                default:
                    throw new NoViableAltException(this);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class AlterTableContext extends ParserRuleContext {

        public TerminalNode ALTER() {
            return getToken(JFSQLParser.ALTER, 0);
        }

        public TerminalNode TABLE() {
            return getToken(JFSQLParser.TABLE, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public RenameTableContext renameTable() {
            return getRuleContext(RenameTableContext.class, 0);
        }

        public RenameColumnContext renameColumn() {
            return getRuleContext(RenameColumnContext.class, 0);
        }

        public AddColumnContext addColumn() {
            return getRuleContext(AddColumnContext.class, 0);
        }

        public DropColumnContext dropColumn() {
            return getRuleContext(DropColumnContext.class, 0);
        }

        public AlterTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_alterTable;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterAlterTable(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitAlterTable(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitAlterTable(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final AlterTableContext alterTable() throws RecognitionException {
        AlterTableContext _localctx = new AlterTableContext(_ctx, getState());
        enterRule(_localctx, 4, RULE_alterTable);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(83);
                match(ALTER);
                setState(84);
                match(TABLE);
                setState(85);
                tableName();
                setState(90);
                _errHandler.sync(this);
                switch (getInterpreter().adaptivePredict(_input, 2, _ctx)) {
                    case 1: {
                        setState(86);
                        renameTable();
                    }
                    break;
                    case 2: {
                        setState(87);
                        renameColumn();
                    }
                    break;
                    case 3: {
                        setState(88);
                        addColumn();
                    }
                    break;
                    case 4: {
                        setState(89);
                        dropColumn();
                    }
                    break;
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class RenameTableContext extends ParserRuleContext {

        public TerminalNode RENAME() {
            return getToken(JFSQLParser.RENAME, 0);
        }

        public TerminalNode TO() {
            return getToken(JFSQLParser.TO, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public RenameTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_renameTable;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterRenameTable(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitRenameTable(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitRenameTable(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final RenameTableContext renameTable() throws RecognitionException {
        RenameTableContext _localctx = new RenameTableContext(_ctx, getState());
        enterRule(_localctx, 6, RULE_renameTable);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(92);
                match(RENAME);
                setState(93);
                match(TO);
                setState(94);
                tableName();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class RenameColumnContext extends ParserRuleContext {

        public TerminalNode RENAME() {
            return getToken(JFSQLParser.RENAME, 0);
        }

        public TerminalNode COLUMN() {
            return getToken(JFSQLParser.COLUMN, 0);
        }

        public List<ColumnNameContext> columnName() {
            return getRuleContexts(ColumnNameContext.class);
        }

        public ColumnNameContext columnName(int i) {
            return getRuleContext(ColumnNameContext.class, i);
        }

        public TerminalNode TO() {
            return getToken(JFSQLParser.TO, 0);
        }

        public RenameColumnContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_renameColumn;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterRenameColumn(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitRenameColumn(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitRenameColumn(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final RenameColumnContext renameColumn() throws RecognitionException {
        RenameColumnContext _localctx = new RenameColumnContext(_ctx, getState());
        enterRule(_localctx, 8, RULE_renameColumn);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(96);
                match(RENAME);
                setState(97);
                match(COLUMN);
                setState(98);
                columnName();
                setState(99);
                match(TO);
                setState(100);
                columnName();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class AddColumnContext extends ParserRuleContext {

        public TerminalNode ADD() {
            return getToken(JFSQLParser.ADD, 0);
        }

        public TerminalNode COLUMN() {
            return getToken(JFSQLParser.COLUMN, 0);
        }

        public ColumnDefinitionContext columnDefinition() {
            return getRuleContext(ColumnDefinitionContext.class, 0);
        }

        public AddColumnContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_addColumn;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterAddColumn(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitAddColumn(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitAddColumn(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final AddColumnContext addColumn() throws RecognitionException {
        AddColumnContext _localctx = new AddColumnContext(_ctx, getState());
        enterRule(_localctx, 10, RULE_addColumn);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(102);
                match(ADD);
                setState(103);
                match(COLUMN);
                setState(104);
                columnDefinition();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class DropColumnContext extends ParserRuleContext {

        public TerminalNode DROP() {
            return getToken(JFSQLParser.DROP, 0);
        }

        public TerminalNode COLUMN() {
            return getToken(JFSQLParser.COLUMN, 0);
        }

        public ColumnNameContext columnName() {
            return getRuleContext(ColumnNameContext.class, 0);
        }

        public DropColumnContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_dropColumn;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterDropColumn(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitDropColumn(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitDropColumn(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final DropColumnContext dropColumn() throws RecognitionException {
        DropColumnContext _localctx = new DropColumnContext(_ctx, getState());
        enterRule(_localctx, 12, RULE_dropColumn);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(106);
                match(DROP);
                setState(107);
                match(COLUMN);
                setState(108);
                columnName();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class CreateTableContext extends ParserRuleContext {

        public TerminalNode CREATE() {
            return getToken(JFSQLParser.CREATE, 0);
        }

        public TerminalNode TABLE() {
            return getToken(JFSQLParser.TABLE, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public TerminalNode OPEN_PAR() {
            return getToken(JFSQLParser.OPEN_PAR, 0);
        }

        public List<ColumnDefinitionContext> columnDefinition() {
            return getRuleContexts(ColumnDefinitionContext.class);
        }

        public ColumnDefinitionContext columnDefinition(int i) {
            return getRuleContext(ColumnDefinitionContext.class, i);
        }

        public TerminalNode CLOSE_PAR() {
            return getToken(JFSQLParser.CLOSE_PAR, 0);
        }

        public IfNotExistsContext ifNotExists() {
            return getRuleContext(IfNotExistsContext.class, 0);
        }

        public List<TerminalNode> COL() {
            return getTokens(JFSQLParser.COL);
        }

        public TerminalNode COL(int i) {
            return getToken(JFSQLParser.COL, i);
        }

        public CreateTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_createTable;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterCreateTable(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitCreateTable(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitCreateTable(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final CreateTableContext createTable() throws RecognitionException {
        CreateTableContext _localctx = new CreateTableContext(_ctx, getState());
        enterRule(_localctx, 14, RULE_createTable);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(110);
                match(CREATE);
                setState(111);
                match(TABLE);
                setState(113);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == IF) {
                    {
                        setState(112);
                        ifNotExists();
                    }
                }

                setState(115);
                tableName();
                setState(116);
                match(OPEN_PAR);
                setState(117);
                columnDefinition();
                setState(122);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(118);
                            match(COL);
                            setState(119);
                            columnDefinition();
                        }
                    }
                    setState(124);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(125);
                match(CLOSE_PAR);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class DeleteContext extends ParserRuleContext {

        public TerminalNode DELETE() {
            return getToken(JFSQLParser.DELETE, 0);
        }

        public TerminalNode FROM() {
            return getToken(JFSQLParser.FROM, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public TerminalNode WHERE() {
            return getToken(JFSQLParser.WHERE, 0);
        }

        public ExprContext expr() {
            return getRuleContext(ExprContext.class, 0);
        }

        public DeleteContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_delete;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterDelete(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitDelete(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitDelete(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final DeleteContext delete() throws RecognitionException {
        DeleteContext _localctx = new DeleteContext(_ctx, getState());
        enterRule(_localctx, 16, RULE_delete);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(127);
                match(DELETE);
                setState(128);
                match(FROM);
                setState(129);
                tableName();
                setState(132);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == WHERE) {
                    {
                        setState(130);
                        match(WHERE);
                        setState(131);
                        expr();
                    }
                }

            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class DropTableContext extends ParserRuleContext {

        public TerminalNode DROP() {
            return getToken(JFSQLParser.DROP, 0);
        }

        public TerminalNode TABLE() {
            return getToken(JFSQLParser.TABLE, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public IfExistsContext ifExists() {
            return getRuleContext(IfExistsContext.class, 0);
        }

        public DropTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_dropTable;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterDropTable(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitDropTable(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitDropTable(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final DropTableContext dropTable() throws RecognitionException {
        DropTableContext _localctx = new DropTableContext(_ctx, getState());
        enterRule(_localctx, 18, RULE_dropTable);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(134);
                match(DROP);
                setState(135);
                match(TABLE);
                setState(137);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == IF) {
                    {
                        setState(136);
                        ifExists();
                    }
                }

                setState(139);
                tableName();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class InsertContext extends ParserRuleContext {

        public TerminalNode INSERT() {
            return getToken(JFSQLParser.INSERT, 0);
        }

        public TerminalNode INTO() {
            return getToken(JFSQLParser.INTO, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public TerminalNode VALUES() {
            return getToken(JFSQLParser.VALUES, 0);
        }

        public List<ValuesInParenthesesContext> valuesInParentheses() {
            return getRuleContexts(ValuesInParenthesesContext.class);
        }

        public ValuesInParenthesesContext valuesInParentheses(int i) {
            return getRuleContext(ValuesInParenthesesContext.class, i);
        }

        public TerminalNode OPEN_PAR() {
            return getToken(JFSQLParser.OPEN_PAR, 0);
        }

        public List<ColumnNameContext> columnName() {
            return getRuleContexts(ColumnNameContext.class);
        }

        public ColumnNameContext columnName(int i) {
            return getRuleContext(ColumnNameContext.class, i);
        }

        public TerminalNode CLOSE_PAR() {
            return getToken(JFSQLParser.CLOSE_PAR, 0);
        }

        public List<TerminalNode> COL() {
            return getTokens(JFSQLParser.COL);
        }

        public TerminalNode COL(int i) {
            return getToken(JFSQLParser.COL, i);
        }

        public InsertContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_insert;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterInsert(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitInsert(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitInsert(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final InsertContext insert() throws RecognitionException {
        InsertContext _localctx = new InsertContext(_ctx, getState());
        enterRule(_localctx, 20, RULE_insert);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(141);
                match(INSERT);
                setState(142);
                match(INTO);
                setState(143);
                tableName();
                setState(155);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == OPEN_PAR) {
                    {
                        setState(144);
                        match(OPEN_PAR);
                        setState(145);
                        columnName();
                        setState(150);
                        _errHandler.sync(this);
                        _la = _input.LA(1);
                        while (_la == COL) {
                            {
                                {
                                    setState(146);
                                    match(COL);
                                    setState(147);
                                    columnName();
                                }
                            }
                            setState(152);
                            _errHandler.sync(this);
                            _la = _input.LA(1);
                        }
                        setState(153);
                        match(CLOSE_PAR);
                    }
                }

                setState(157);
                match(VALUES);
                setState(158);
                valuesInParentheses();
                setState(163);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(159);
                            match(COL);
                            setState(160);
                            valuesInParentheses();
                        }
                    }
                    setState(165);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class SelectContext extends ParserRuleContext {

        public TerminalNode SELECT() {
            return getToken(JFSQLParser.SELECT, 0);
        }

        public List<ColumnNameContext> columnName() {
            return getRuleContexts(ColumnNameContext.class);
        }

        public ColumnNameContext columnName(int i) {
            return getRuleContext(ColumnNameContext.class, i);
        }

        public TerminalNode FROM() {
            return getToken(JFSQLParser.FROM, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public List<TerminalNode> COL() {
            return getTokens(JFSQLParser.COL);
        }

        public TerminalNode COL(int i) {
            return getToken(JFSQLParser.COL, i);
        }

        public List<JoinOperationContext> joinOperation() {
            return getRuleContexts(JoinOperationContext.class);
        }

        public JoinOperationContext joinOperation(int i) {
            return getRuleContext(JoinOperationContext.class, i);
        }

        public TerminalNode WHERE() {
            return getToken(JFSQLParser.WHERE, 0);
        }

        public ExprContext expr() {
            return getRuleContext(ExprContext.class, 0);
        }

        public OrderByContext orderBy() {
            return getRuleContext(OrderByContext.class, 0);
        }

        public LimitContext limit() {
            return getRuleContext(LimitContext.class, 0);
        }

        public SelectContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_select;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterSelect(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitSelect(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitSelect(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final SelectContext select() throws RecognitionException {
        SelectContext _localctx = new SelectContext(_ctx, getState());
        enterRule(_localctx, 22, RULE_select);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(166);
                match(SELECT);
                setState(167);
                columnName();
                setState(172);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(168);
                            match(COL);
                            setState(169);
                            columnName();
                        }
                    }
                    setState(174);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(175);
                match(FROM);
                setState(176);
                tableName();
                setState(180);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 492581209243648L) != 0)) {
                    {
                        {
                            setState(177);
                            joinOperation();
                        }
                    }
                    setState(182);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(185);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == WHERE) {
                    {
                        setState(183);
                        match(WHERE);
                        setState(184);
                        expr();
                    }
                }

                setState(188);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == ORDER) {
                    {
                        setState(187);
                        orderBy();
                    }
                }

                setState(191);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == LIMIT) {
                    {
                        setState(190);
                        limit();
                    }
                }

            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class OrderByContext extends ParserRuleContext {

        public TerminalNode ORDER() {
            return getToken(JFSQLParser.ORDER, 0);
        }

        public TerminalNode BY() {
            return getToken(JFSQLParser.BY, 0);
        }

        public ColumnNameContext columnName() {
            return getRuleContext(ColumnNameContext.class, 0);
        }

        public OrderingContext ordering() {
            return getRuleContext(OrderingContext.class, 0);
        }

        public OrderByContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_orderBy;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterOrderBy(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitOrderBy(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitOrderBy(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final OrderByContext orderBy() throws RecognitionException {
        OrderByContext _localctx = new OrderByContext(_ctx, getState());
        enterRule(_localctx, 24, RULE_orderBy);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(193);
                match(ORDER);
                setState(194);
                match(BY);
                setState(195);
                columnName();
                setState(197);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == ASC || _la == DESC) {
                    {
                        setState(196);
                        ordering();
                    }
                }

            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class OrderingContext extends ParserRuleContext {

        public TerminalNode ASC() {
            return getToken(JFSQLParser.ASC, 0);
        }

        public TerminalNode DESC() {
            return getToken(JFSQLParser.DESC, 0);
        }

        public OrderingContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_ordering;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterOrdering(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitOrdering(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitOrdering(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final OrderingContext ordering() throws RecognitionException {
        OrderingContext _localctx = new OrderingContext(_ctx, getState());
        enterRule(_localctx, 26, RULE_ordering);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(199);
                _la = _input.LA(1);
                if (!(_la == ASC || _la == DESC)) {
                    _errHandler.recoverInline(this);
                } else {
									if (_input.LA(1) == Token.EOF) {
										matchedEOF = true;
									}
                    _errHandler.reportMatch(this);
                    consume();
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class LimitContext extends ParserRuleContext {

        public TerminalNode LIMIT() {
            return getToken(JFSQLParser.LIMIT, 0);
        }

        public NumericValueContext numericValue() {
            return getRuleContext(NumericValueContext.class, 0);
        }

        public OffsetContext offset() {
            return getRuleContext(OffsetContext.class, 0);
        }

        public LimitContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_limit;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterLimit(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitLimit(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitLimit(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final LimitContext limit() throws RecognitionException {
        LimitContext _localctx = new LimitContext(_ctx, getState());
        enterRule(_localctx, 28, RULE_limit);
        try {
            setState(207);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 16, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(201);
                    match(LIMIT);
                    setState(202);
                    numericValue();
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(203);
                    match(LIMIT);
                    setState(204);
                    numericValue();
                    setState(205);
                    offset();
                }
                break;
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class OffsetContext extends ParserRuleContext {

        public TerminalNode OFFSET() {
            return getToken(JFSQLParser.OFFSET, 0);
        }

        public NumericValueContext numericValue() {
            return getRuleContext(NumericValueContext.class, 0);
        }

        public OffsetContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_offset;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterOffset(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitOffset(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitOffset(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final OffsetContext offset() throws RecognitionException {
        OffsetContext _localctx = new OffsetContext(_ctx, getState());
        enterRule(_localctx, 30, RULE_offset);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(209);
                match(OFFSET);
                setState(210);
                numericValue();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class NumericValueContext extends ParserRuleContext {

        public TerminalNode NUMERIC_LITERAL() {
            return getToken(JFSQLParser.NUMERIC_LITERAL, 0);
        }

        public NumericValueContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_numericValue;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterNumericValue(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitNumericValue(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitNumericValue(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final NumericValueContext numericValue() throws RecognitionException {
        NumericValueContext _localctx = new NumericValueContext(_ctx, getState());
        enterRule(_localctx, 32, RULE_numericValue);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(212);
                match(NUMERIC_LITERAL);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class JoinOperationContext extends ParserRuleContext {

        public InnerJoinContext innerJoin() {
            return getRuleContext(InnerJoinContext.class, 0);
        }

        public LeftJoinContext leftJoin() {
            return getRuleContext(LeftJoinContext.class, 0);
        }

        public JoinOperationContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_joinOperation;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterJoinOperation(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitJoinOperation(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitJoinOperation(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final JoinOperationContext joinOperation() throws RecognitionException {
        JoinOperationContext _localctx = new JoinOperationContext(_ctx, getState());
        enterRule(_localctx, 34, RULE_joinOperation);
        try {
            setState(216);
            _errHandler.sync(this);
            switch (_input.LA(1)) {
                case JOIN:
                case INNER:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(214);
                    innerJoin();
                }
                break;
                case LEFT:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(215);
                    leftJoin();
                }
                break;
                default:
                    throw new NoViableAltException(this);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class InnerJoinContext extends ParserRuleContext {

        public TerminalNode JOIN() {
            return getToken(JFSQLParser.JOIN, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public TerminalNode ON() {
            return getToken(JFSQLParser.ON, 0);
        }

        public List<TableDotColumnNameContext> tableDotColumnName() {
            return getRuleContexts(TableDotColumnNameContext.class);
        }

        public TableDotColumnNameContext tableDotColumnName(int i) {
            return getRuleContext(TableDotColumnNameContext.class, i);
        }

        public TerminalNode EQ() {
            return getToken(JFSQLParser.EQ, 0);
        }

        public TerminalNode INNER() {
            return getToken(JFSQLParser.INNER, 0);
        }

        public InnerJoinContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_innerJoin;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterInnerJoin(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitInnerJoin(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitInnerJoin(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final InnerJoinContext innerJoin() throws RecognitionException {
        InnerJoinContext _localctx = new InnerJoinContext(_ctx, getState());
        enterRule(_localctx, 36, RULE_innerJoin);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(219);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == INNER) {
                    {
                        setState(218);
                        match(INNER);
                    }
                }

                setState(221);
                match(JOIN);
                setState(222);
                tableName();
                setState(223);
                match(ON);
                setState(224);
                tableDotColumnName();
                setState(225);
                match(EQ);
                setState(226);
                tableDotColumnName();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class LeftJoinContext extends ParserRuleContext {

        public TerminalNode LEFT() {
            return getToken(JFSQLParser.LEFT, 0);
        }

        public TerminalNode JOIN() {
            return getToken(JFSQLParser.JOIN, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public TerminalNode ON() {
            return getToken(JFSQLParser.ON, 0);
        }

        public List<TableDotColumnNameContext> tableDotColumnName() {
            return getRuleContexts(TableDotColumnNameContext.class);
        }

        public TableDotColumnNameContext tableDotColumnName(int i) {
            return getRuleContext(TableDotColumnNameContext.class, i);
        }

        public TerminalNode EQ() {
            return getToken(JFSQLParser.EQ, 0);
        }

        public TerminalNode OUTER() {
            return getToken(JFSQLParser.OUTER, 0);
        }

        public LeftJoinContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_leftJoin;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterLeftJoin(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitLeftJoin(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitLeftJoin(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final LeftJoinContext leftJoin() throws RecognitionException {
        LeftJoinContext _localctx = new LeftJoinContext(_ctx, getState());
        enterRule(_localctx, 38, RULE_leftJoin);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(228);
                match(LEFT);
                setState(230);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == OUTER) {
                    {
                        setState(229);
                        match(OUTER);
                    }
                }

                setState(232);
                match(JOIN);
                setState(233);
                tableName();
                setState(234);
                match(ON);
                setState(235);
                tableDotColumnName();
                setState(236);
                match(EQ);
                setState(237);
                tableDotColumnName();
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class UpdateContext extends ParserRuleContext {

        public TerminalNode UPDATE() {
            return getToken(JFSQLParser.UPDATE, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
        }

        public TerminalNode SET() {
            return getToken(JFSQLParser.SET, 0);
        }

        public List<ColumnNameContext> columnName() {
            return getRuleContexts(ColumnNameContext.class);
        }

        public ColumnNameContext columnName(int i) {
            return getRuleContext(ColumnNameContext.class, i);
        }

        public List<TerminalNode> EQ() {
            return getTokens(JFSQLParser.EQ);
        }

        public TerminalNode EQ(int i) {
            return getToken(JFSQLParser.EQ, i);
        }

        public List<ValueContext> value() {
            return getRuleContexts(ValueContext.class);
        }

        public ValueContext value(int i) {
            return getRuleContext(ValueContext.class, i);
        }

        public List<TerminalNode> COL() {
            return getTokens(JFSQLParser.COL);
        }

        public TerminalNode COL(int i) {
            return getToken(JFSQLParser.COL, i);
        }

        public TerminalNode WHERE() {
            return getToken(JFSQLParser.WHERE, 0);
        }

        public ExprContext expr() {
            return getRuleContext(ExprContext.class, 0);
        }

        public UpdateContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_update;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterUpdate(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitUpdate(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitUpdate(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final UpdateContext update() throws RecognitionException {
        UpdateContext _localctx = new UpdateContext(_ctx, getState());
        enterRule(_localctx, 40, RULE_update);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(239);
                match(UPDATE);
                setState(240);
                tableName();
                setState(241);
                match(SET);
                setState(242);
                columnName();
                setState(243);
                match(EQ);
                setState(244);
                value();
                setState(252);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(245);
                            match(COL);
                            setState(246);
                            columnName();
                            setState(247);
                            match(EQ);
                            setState(248);
                            value();
                        }
                    }
                    setState(254);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(257);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == WHERE) {
                    {
                        setState(255);
                        match(WHERE);
                        setState(256);
                        expr();
                    }
                }

            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class ColumnDefinitionContext extends ParserRuleContext {

        public ColumnNameContext columnName() {
            return getRuleContext(ColumnNameContext.class, 0);
        }

        public ColumnTypeContext columnType() {
            return getRuleContext(ColumnTypeContext.class, 0);
        }

        public NotNullContext notNull() {
            return getRuleContext(NotNullContext.class, 0);
        }

        public ColumnDefinitionContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_columnDefinition;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterColumnDefinition(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitColumnDefinition(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitColumnDefinition(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final ColumnDefinitionContext columnDefinition() throws RecognitionException {
        ColumnDefinitionContext _localctx = new ColumnDefinitionContext(_ctx, getState());
        enterRule(_localctx, 42, RULE_columnDefinition);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(259);
                columnName();
                setState(260);
                columnType();
                setState(262);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == NOT) {
                    {
                        setState(261);
                        notNull();
                    }
                }

            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class NotNullContext extends ParserRuleContext {

        public TerminalNode NOT() {
            return getToken(JFSQLParser.NOT, 0);
        }

        public TerminalNode NULL() {
            return getToken(JFSQLParser.NULL, 0);
        }

        public NotNullContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_notNull;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterNotNull(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitNotNull(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitNotNull(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final NotNullContext notNull() throws RecognitionException {
        NotNullContext _localctx = new NotNullContext(_ctx, getState());
        enterRule(_localctx, 44, RULE_notNull);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(264);
                match(NOT);
                setState(265);
                match(NULL);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class IfExistsContext extends ParserRuleContext {

        public TerminalNode IF() {
            return getToken(JFSQLParser.IF, 0);
        }

        public TerminalNode EXISTS() {
            return getToken(JFSQLParser.EXISTS, 0);
        }

        public IfExistsContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_ifExists;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterIfExists(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitIfExists(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitIfExists(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final IfExistsContext ifExists() throws RecognitionException {
        IfExistsContext _localctx = new IfExistsContext(_ctx, getState());
        enterRule(_localctx, 46, RULE_ifExists);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(267);
                match(IF);
                setState(268);
                match(EXISTS);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class IfNotExistsContext extends ParserRuleContext {

        public TerminalNode IF() {
            return getToken(JFSQLParser.IF, 0);
        }

        public TerminalNode NOT() {
            return getToken(JFSQLParser.NOT, 0);
        }

        public TerminalNode EXISTS() {
            return getToken(JFSQLParser.EXISTS, 0);
        }

        public IfNotExistsContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_ifNotExists;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterIfNotExists(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitIfNotExists(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitIfNotExists(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final IfNotExistsContext ifNotExists() throws RecognitionException {
        IfNotExistsContext _localctx = new IfNotExistsContext(_ctx, getState());
        enterRule(_localctx, 48, RULE_ifNotExists);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(270);
                match(IF);
                setState(271);
                match(NOT);
                setState(272);
                match(EXISTS);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class ColumnTypeContext extends ParserRuleContext {

        public TerminalNode INTEGER() {
            return getToken(JFSQLParser.INTEGER, 0);
        }

        public TerminalNode REAL() {
            return getToken(JFSQLParser.REAL, 0);
        }

        public TerminalNode TEXT() {
            return getToken(JFSQLParser.TEXT, 0);
        }

        public TerminalNode BLOB() {
            return getToken(JFSQLParser.BLOB, 0);
        }

        public ColumnTypeContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_columnType;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterColumnType(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitColumnType(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitColumnType(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final ColumnTypeContext columnType() throws RecognitionException {
        ColumnTypeContext _localctx = new ColumnTypeContext(_ctx, getState());
        enterRule(_localctx, 50, RULE_columnType);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(274);
                _la = _input.LA(1);
                if (!((((_la) & ~0x3f) == 0 && ((1L << _la) & 16888498602639360L) != 0))) {
                    _errHandler.recoverInline(this);
                } else {
									if (_input.LA(1) == Token.EOF) {
										matchedEOF = true;
									}
                    _errHandler.reportMatch(this);
                    consume();
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class ExprContext extends ParserRuleContext {

        public List<ColumnNameContext> columnName() {
            return getRuleContexts(ColumnNameContext.class);
        }

        public ColumnNameContext columnName(int i) {
            return getRuleContext(ColumnNameContext.class, i);
        }

        public List<SymbolContext> symbol() {
            return getRuleContexts(SymbolContext.class);
        }

        public SymbolContext symbol(int i) {
            return getRuleContext(SymbolContext.class, i);
        }

        public List<ValueContext> value() {
            return getRuleContexts(ValueContext.class);
        }

        public ValueContext value(int i) {
            return getRuleContext(ValueContext.class, i);
        }

        public List<BinaryOperatorContext> binaryOperator() {
            return getRuleContexts(BinaryOperatorContext.class);
        }

        public BinaryOperatorContext binaryOperator(int i) {
            return getRuleContext(BinaryOperatorContext.class, i);
        }

        public ExprContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_expr;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterExpr(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitExpr(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitExpr(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final ExprContext expr() throws RecognitionException {
        ExprContext _localctx = new ExprContext(_ctx, getState());
        enterRule(_localctx, 52, RULE_expr);
        int _la;
        try {
            setState(293);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 24, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(276);
                    columnName();
                    setState(277);
                    symbol();
                    setState(278);
                    value();
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(280);
                    columnName();
                    setState(281);
                    symbol();
                    setState(282);
                    value();
                    setState(290);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                    while (_la == AND || _la == OR) {
                        {
                            {
                                setState(283);
                                binaryOperator();
                                setState(284);
                                columnName();
                                setState(285);
                                symbol();
                                setState(286);
                                value();
                            }
                        }
                        setState(292);
                        _errHandler.sync(this);
                        _la = _input.LA(1);
                    }
                }
                break;
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class BinaryOperatorContext extends ParserRuleContext {

        public TerminalNode AND() {
            return getToken(JFSQLParser.AND, 0);
        }

        public TerminalNode OR() {
            return getToken(JFSQLParser.OR, 0);
        }

        public BinaryOperatorContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_binaryOperator;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterBinaryOperator(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitBinaryOperator(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitBinaryOperator(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final BinaryOperatorContext binaryOperator() throws RecognitionException {
        BinaryOperatorContext _localctx = new BinaryOperatorContext(_ctx, getState());
        enterRule(_localctx, 54, RULE_binaryOperator);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(295);
                _la = _input.LA(1);
                if (!(_la == AND || _la == OR)) {
                    _errHandler.recoverInline(this);
                } else {
									if (_input.LA(1) == Token.EOF) {
										matchedEOF = true;
									}
                    _errHandler.reportMatch(this);
                    consume();
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class SymbolContext extends ParserRuleContext {

        public TerminalNode EQ() {
            return getToken(JFSQLParser.EQ, 0);
        }

        public TerminalNode NOT_EQ() {
            return getToken(JFSQLParser.NOT_EQ, 0);
        }

        public TerminalNode LT() {
            return getToken(JFSQLParser.LT, 0);
        }

        public TerminalNode LT_EQ() {
            return getToken(JFSQLParser.LT_EQ, 0);
        }

        public TerminalNode GT() {
            return getToken(JFSQLParser.GT, 0);
        }

        public TerminalNode GT_EQ() {
            return getToken(JFSQLParser.GT_EQ, 0);
        }

        public TerminalNode LIKE() {
            return getToken(JFSQLParser.LIKE, 0);
        }

        public SymbolContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_symbol;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterSymbol(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitSymbol(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitSymbol(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final SymbolContext symbol() throws RecognitionException {
        SymbolContext _localctx = new SymbolContext(_ctx, getState());
        enterRule(_localctx, 56, RULE_symbol);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(297);
                _la = _input.LA(1);
                if (!((((_la) & ~0x3f) == 0 && ((1L << _la) & 270208L) != 0))) {
                    _errHandler.recoverInline(this);
                } else {
									if (_input.LA(1) == Token.EOF) {
										matchedEOF = true;
									}
                    _errHandler.reportMatch(this);
                    consume();
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class ValueContext extends ParserRuleContext {

        public TerminalNode NUMERIC_LITERAL() {
            return getToken(JFSQLParser.NUMERIC_LITERAL, 0);
        }

        public TerminalNode STRING_LITERAL() {
            return getToken(JFSQLParser.STRING_LITERAL, 0);
        }

        public TerminalNode QUESTION_MARK() {
            return getToken(JFSQLParser.QUESTION_MARK, 0);
        }

        public ValueContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_value;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterValue(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitValue(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitValue(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final ValueContext value() throws RecognitionException {
        ValueContext _localctx = new ValueContext(_ctx, getState());
        enterRule(_localctx, 58, RULE_value);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(299);
                _la = _input.LA(1);
                if (!((((_la) & ~0x3f) == 0 && ((1L << _la) & 108086391056900098L) != 0))) {
                    _errHandler.recoverInline(this);
                } else {
									if (_input.LA(1) == Token.EOF) {
										matchedEOF = true;
									}
                    _errHandler.reportMatch(this);
                    consume();
                }
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class ValuesInParenthesesContext extends ParserRuleContext {

        public TerminalNode OPEN_PAR() {
            return getToken(JFSQLParser.OPEN_PAR, 0);
        }

        public List<ValueContext> value() {
            return getRuleContexts(ValueContext.class);
        }

        public ValueContext value(int i) {
            return getRuleContext(ValueContext.class, i);
        }

        public TerminalNode CLOSE_PAR() {
            return getToken(JFSQLParser.CLOSE_PAR, 0);
        }

        public List<TerminalNode> COL() {
            return getTokens(JFSQLParser.COL);
        }

        public TerminalNode COL(int i) {
            return getToken(JFSQLParser.COL, i);
        }

        public ValuesInParenthesesContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_valuesInParentheses;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterValuesInParentheses(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitValuesInParentheses(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitValuesInParentheses(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final ValuesInParenthesesContext valuesInParentheses() throws RecognitionException {
        ValuesInParenthesesContext _localctx = new ValuesInParenthesesContext(_ctx, getState());
        enterRule(_localctx, 60, RULE_valuesInParentheses);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(301);
                match(OPEN_PAR);
                setState(302);
                value();
                setState(307);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(303);
                            match(COL);
                            setState(304);
                            value();
                        }
                    }
                    setState(309);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(310);
                match(CLOSE_PAR);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class TableNameContext extends ParserRuleContext {

        public TerminalNode IDENTIFIER() {
            return getToken(JFSQLParser.IDENTIFIER, 0);
        }

        public TableNameContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_tableName;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterTableName(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitTableName(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitTableName(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final TableNameContext tableName() throws RecognitionException {
        TableNameContext _localctx = new TableNameContext(_ctx, getState());
        enterRule(_localctx, 62, RULE_tableName);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(312);
                match(IDENTIFIER);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class TableDotColumnNameContext extends ParserRuleContext {

        public List<TerminalNode> IDENTIFIER() {
            return getTokens(JFSQLParser.IDENTIFIER);
        }

        public TerminalNode IDENTIFIER(int i) {
            return getToken(JFSQLParser.IDENTIFIER, i);
        }

        public TerminalNode DOT() {
            return getToken(JFSQLParser.DOT, 0);
        }

        public TableDotColumnNameContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_tableDotColumnName;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterTableDotColumnName(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitTableDotColumnName(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitTableDotColumnName(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final TableDotColumnNameContext tableDotColumnName() throws RecognitionException {
        TableDotColumnNameContext _localctx = new TableDotColumnNameContext(_ctx, getState());
        enterRule(_localctx, 64, RULE_tableDotColumnName);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(314);
                match(IDENTIFIER);
                setState(315);
                match(DOT);
                setState(316);
                match(IDENTIFIER);
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    @SuppressWarnings("CheckReturnValue")
    public static class ColumnNameContext extends ParserRuleContext {

        public List<TerminalNode> IDENTIFIER() {
            return getTokens(JFSQLParser.IDENTIFIER);
        }

        public TerminalNode IDENTIFIER(int i) {
            return getToken(JFSQLParser.IDENTIFIER, i);
        }

        public TerminalNode DOT() {
            return getToken(JFSQLParser.DOT, 0);
        }

        public ColumnNameContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        @Override
        public int getRuleIndex() {
            return RULE_columnName;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterColumnName(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitColumnName(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitColumnName(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    public final ColumnNameContext columnName() throws RecognitionException {
        ColumnNameContext _localctx = new ColumnNameContext(_ctx, getState());
        enterRule(_localctx, 66, RULE_columnName);
        try {
            setState(322);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 26, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(318);
                    match(IDENTIFIER);
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(319);
                    match(IDENTIFIER);
                    setState(320);
                    match(DOT);
                    setState(321);
                    match(IDENTIFIER);
                }
                break;
            }
        } catch (RecognitionException re) {
            _localctx.exception = re;
            _errHandler.reportError(this, re);
            _errHandler.recover(this, re);
        } finally {
            exitRule();
        }
        return _localctx;
    }

    public static final String _serializedATN =
        "\u0004\u00019\u0145\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002" +
            "\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002" +
            "\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002" +
            "\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002" +
            "\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f" +
            "\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012" +
            "\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015" +
            "\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007\u0018" +
            "\u0002\u0019\u0007\u0019\u0002\u001a\u0007\u001a\u0002\u001b\u0007\u001b" +
            "\u0002\u001c\u0007\u001c\u0002\u001d\u0007\u001d\u0002\u001e\u0007\u001e" +
            "\u0002\u001f\u0007\u001f\u0002 \u0007 \u0002!\u0007!\u0001\u0000\u0001" +
            "\u0000\u0003\u0000G\b\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001" +
            "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0003" +
            "\u0001R\b\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001" +
            "\u0002\u0001\u0002\u0001\u0002\u0003\u0002[\b\u0002\u0001\u0003\u0001" +
            "\u0003\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0001" +
            "\u0004\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001" +
            "\u0005\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0007\u0001" +
            "\u0007\u0001\u0007\u0003\u0007r\b\u0007\u0001\u0007\u0001\u0007\u0001" +
            "\u0007\u0001\u0007\u0001\u0007\u0005\u0007y\b\u0007\n\u0007\f\u0007|\t" +
            "\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b" +
            "\u0003\b\u0085\b\b\u0001\t\u0001\t\u0001\t\u0003\t\u008a\b\t\u0001\t\u0001" +
            "\t\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0005\n\u0095" +
            "\b\n\n\n\f\n\u0098\t\n\u0001\n\u0001\n\u0003\n\u009c\b\n\u0001\n\u0001" +
            "\n\u0001\n\u0001\n\u0005\n\u00a2\b\n\n\n\f\n\u00a5\t\n\u0001\u000b\u0001" +
            "\u000b\u0001\u000b\u0001\u000b\u0005\u000b\u00ab\b\u000b\n\u000b\f\u000b" +
            "\u00ae\t\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0005\u000b\u00b3\b" +
            "\u000b\n\u000b\f\u000b\u00b6\t\u000b\u0001\u000b\u0001\u000b\u0003\u000b" +
            "\u00ba\b\u000b\u0001\u000b\u0003\u000b\u00bd\b\u000b\u0001\u000b\u0003" +
            "\u000b\u00c0\b\u000b\u0001\f\u0001\f\u0001\f\u0001\f\u0003\f\u00c6\b\f" +
            "\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e\u0001\u000e\u0001" +
            "\u000e\u0001\u000e\u0003\u000e\u00d0\b\u000e\u0001\u000f\u0001\u000f\u0001" +
            "\u000f\u0001\u0010\u0001\u0010\u0001\u0011\u0001\u0011\u0003\u0011\u00d9" +
            "\b\u0011\u0001\u0012\u0003\u0012\u00dc\b\u0012\u0001\u0012\u0001\u0012" +
            "\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0013" +
            "\u0001\u0013\u0003\u0013\u00e7\b\u0013\u0001\u0013\u0001\u0013\u0001\u0013" +
            "\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0014\u0001\u0014" +
            "\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014" +
            "\u0001\u0014\u0001\u0014\u0001\u0014\u0005\u0014\u00fb\b\u0014\n\u0014" +
            "\f\u0014\u00fe\t\u0014\u0001\u0014\u0001\u0014\u0003\u0014\u0102\b\u0014" +
            "\u0001\u0015\u0001\u0015\u0001\u0015\u0003\u0015\u0107\b\u0015\u0001\u0016" +
            "\u0001\u0016\u0001\u0016\u0001\u0017\u0001\u0017\u0001\u0017\u0001\u0018" +
            "\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0019\u0001\u0019\u0001\u001a" +
            "\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a" +
            "\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0005\u001a" +
            "\u0121\b\u001a\n\u001a\f\u001a\u0124\t\u001a\u0003\u001a\u0126\b\u001a" +
            "\u0001\u001b\u0001\u001b\u0001\u001c\u0001\u001c\u0001\u001d\u0001\u001d" +
            "\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0005\u001e\u0132\b\u001e" +
            "\n\u001e\f\u001e\u0135\t\u001e\u0001\u001e\u0001\u001e\u0001\u001f\u0001" +
            "\u001f\u0001 \u0001 \u0001 \u0001 \u0001!\u0001!\u0001!\u0001!\u0003!" +
            "\u0143\b!\u0001!\u0000\u0000\"\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010" +
            "\u0012\u0014\u0016\u0018\u001a\u001c\u001e \"$&(*,.02468:<>@B\u0000\u0005" +
            "\u0001\u0000\u0017\u0018\u0001\u000025\u0001\u0000\u0019\u001a\u0002\u0000" +
            "\u0007\f\u0012\u0012\u0003\u0000\u0001\u0001\r\r78\u0144\u0000D\u0001" +
            "\u0000\u0000\u0000\u0002Q\u0001\u0000\u0000\u0000\u0004S\u0001\u0000\u0000" +
            "\u0000\u0006\\\u0001\u0000\u0000\u0000\b`\u0001\u0000\u0000\u0000\nf\u0001" +
            "\u0000\u0000\u0000\fj\u0001\u0000\u0000\u0000\u000en\u0001\u0000\u0000" +
            "\u0000\u0010\u007f\u0001\u0000\u0000\u0000\u0012\u0086\u0001\u0000\u0000" +
            "\u0000\u0014\u008d\u0001\u0000\u0000\u0000\u0016\u00a6\u0001\u0000\u0000" +
            "\u0000\u0018\u00c1\u0001\u0000\u0000\u0000\u001a\u00c7\u0001\u0000\u0000" +
            "\u0000\u001c\u00cf\u0001\u0000\u0000\u0000\u001e\u00d1\u0001\u0000\u0000" +
            "\u0000 \u00d4\u0001\u0000\u0000\u0000\"\u00d8\u0001\u0000\u0000\u0000" +
            "$\u00db\u0001\u0000\u0000\u0000&\u00e4\u0001\u0000\u0000\u0000(\u00ef" +
            "\u0001\u0000\u0000\u0000*\u0103\u0001\u0000\u0000\u0000,\u0108\u0001\u0000" +
            "\u0000\u0000.\u010b\u0001\u0000\u0000\u00000\u010e\u0001\u0000\u0000\u0000" +
            "2\u0112\u0001\u0000\u0000\u00004\u0125\u0001\u0000\u0000\u00006\u0127" +
            "\u0001\u0000\u0000\u00008\u0129\u0001\u0000\u0000\u0000:\u012b\u0001\u0000" +
            "\u0000\u0000<\u012d\u0001\u0000\u0000\u0000>\u0138\u0001\u0000\u0000\u0000" +
            "@\u013a\u0001\u0000\u0000\u0000B\u0142\u0001\u0000\u0000\u0000DF\u0003" +
            "\u0002\u0001\u0000EG\u0005\u0003\u0000\u0000FE\u0001\u0000\u0000\u0000" +
            "FG\u0001\u0000\u0000\u0000GH\u0001\u0000\u0000\u0000HI\u0005\u0000\u0000" +
            "\u0001I\u0001\u0001\u0000\u0000\u0000JR\u0003\u0004\u0002\u0000KR\u0003" +
            "\u000e\u0007\u0000LR\u0003\u0010\b\u0000MR\u0003\u0012\t\u0000NR\u0003" +
            "\u0014\n\u0000OR\u0003\u0016\u000b\u0000PR\u0003(\u0014\u0000QJ\u0001" +
            "\u0000\u0000\u0000QK\u0001\u0000\u0000\u0000QL\u0001\u0000\u0000\u0000" +
            "QM\u0001\u0000\u0000\u0000QN\u0001\u0000\u0000\u0000QO\u0001\u0000\u0000" +
            "\u0000QP\u0001\u0000\u0000\u0000R\u0003\u0001\u0000\u0000\u0000ST\u0005" +
            "\u001c\u0000\u0000TU\u0005\'\u0000\u0000UZ\u0003>\u001f\u0000V[\u0003" +
            "\u0006\u0003\u0000W[\u0003\b\u0004\u0000X[\u0003\n\u0005\u0000Y[\u0003" +
            "\f\u0006\u0000ZV\u0001\u0000\u0000\u0000ZW\u0001\u0000\u0000\u0000ZX\u0001" +
            "\u0000\u0000\u0000ZY\u0001\u0000\u0000\u0000[\u0005\u0001\u0000\u0000" +
            "\u0000\\]\u0005+\u0000\u0000]^\u0005,\u0000\u0000^_\u0003>\u001f\u0000" +
            "_\u0007\u0001\u0000\u0000\u0000`a\u0005+\u0000\u0000ab\u0005\u001e\u0000" +
            "\u0000bc\u0003B!\u0000cd\u0005,\u0000\u0000de\u0003B!\u0000e\t\u0001\u0000" +
            "\u0000\u0000fg\u0005\u001b\u0000\u0000gh\u0005\u001e\u0000\u0000hi\u0003" +
            "*\u0015\u0000i\u000b\u0001\u0000\u0000\u0000jk\u0005!\u0000\u0000kl\u0005" +
            "\u001e\u0000\u0000lm\u0003B!\u0000m\r\u0001\u0000\u0000\u0000no\u0005" +
            "\u001d\u0000\u0000oq\u0005\'\u0000\u0000pr\u00030\u0018\u0000qp\u0001" +
            "\u0000\u0000\u0000qr\u0001\u0000\u0000\u0000rs\u0001\u0000\u0000\u0000" +
            "st\u0003>\u001f\u0000tu\u0005\u0005\u0000\u0000uz\u0003*\u0015\u0000v" +
            "w\u0005\u0002\u0000\u0000wy\u0003*\u0015\u0000xv\u0001\u0000\u0000\u0000" +
            "y|\u0001\u0000\u0000\u0000zx\u0001\u0000\u0000\u0000z{\u0001\u0000\u0000" +
            "\u0000{}\u0001\u0000\u0000\u0000|z\u0001\u0000\u0000\u0000}~\u0005\u0006" +
            "\u0000\u0000~\u000f\u0001\u0000\u0000\u0000\u007f\u0080\u0005\u001f\u0000" +
            "\u0000\u0080\u0081\u0005\"\u0000\u0000\u0081\u0084\u0003>\u001f\u0000" +
            "\u0082\u0083\u0005*\u0000\u0000\u0083\u0085\u00034\u001a\u0000\u0084\u0082" +
            "\u0001\u0000\u0000\u0000\u0084\u0085\u0001\u0000\u0000\u0000\u0085\u0011" +
            "\u0001\u0000\u0000\u0000\u0086\u0087\u0005!\u0000\u0000\u0087\u0089\u0005" +
            "\'\u0000\u0000\u0088\u008a\u0003.\u0017\u0000\u0089\u0088\u0001\u0000" +
            "\u0000\u0000\u0089\u008a\u0001\u0000\u0000\u0000\u008a\u008b\u0001\u0000" +
            "\u0000\u0000\u008b\u008c\u0003>\u001f\u0000\u008c\u0013\u0001\u0000\u0000" +
            "\u0000\u008d\u008e\u0005#\u0000\u0000\u008e\u008f\u0005$\u0000\u0000\u008f" +
            "\u009b\u0003>\u001f\u0000\u0090\u0091\u0005\u0005\u0000\u0000\u0091\u0096" +
            "\u0003B!\u0000\u0092\u0093\u0005\u0002\u0000\u0000\u0093\u0095\u0003B" +
            "!\u0000\u0094\u0092\u0001\u0000\u0000\u0000\u0095\u0098\u0001\u0000\u0000" +
            "\u0000\u0096\u0094\u0001\u0000\u0000\u0000\u0096\u0097\u0001\u0000\u0000" +
            "\u0000\u0097\u0099\u0001\u0000\u0000\u0000\u0098\u0096\u0001\u0000\u0000" +
            "\u0000\u0099\u009a\u0005\u0006\u0000\u0000\u009a\u009c\u0001\u0000\u0000" +
            "\u0000\u009b\u0090\u0001\u0000\u0000\u0000\u009b\u009c\u0001\u0000\u0000" +
            "\u0000\u009c\u009d\u0001\u0000\u0000\u0000\u009d\u009e\u0005)\u0000\u0000" +
            "\u009e\u00a3\u0003<\u001e\u0000\u009f\u00a0\u0005\u0002\u0000\u0000\u00a0" +
            "\u00a2\u0003<\u001e\u0000\u00a1\u009f\u0001\u0000\u0000\u0000\u00a2\u00a5" +
            "\u0001\u0000\u0000\u0000\u00a3\u00a1\u0001\u0000\u0000\u0000\u00a3\u00a4" +
            "\u0001\u0000\u0000\u0000\u00a4\u0015\u0001\u0000\u0000\u0000\u00a5\u00a3" +
            "\u0001\u0000\u0000\u0000\u00a6\u00a7\u0005%\u0000\u0000\u00a7\u00ac\u0003" +
            "B!\u0000\u00a8\u00a9\u0005\u0002\u0000\u0000\u00a9\u00ab\u0003B!\u0000" +
            "\u00aa\u00a8\u0001\u0000\u0000\u0000\u00ab\u00ae\u0001\u0000\u0000\u0000" +
            "\u00ac\u00aa\u0001\u0000\u0000\u0000\u00ac\u00ad\u0001\u0000\u0000\u0000" +
            "\u00ad\u00af\u0001\u0000\u0000\u0000\u00ae\u00ac\u0001\u0000\u0000\u0000" +
            "\u00af\u00b0\u0005\"\u0000\u0000\u00b0\u00b4\u0003>\u001f\u0000\u00b1" +
            "\u00b3\u0003\"\u0011\u0000\u00b2\u00b1\u0001\u0000\u0000\u0000\u00b3\u00b6" +
            "\u0001\u0000\u0000\u0000\u00b4\u00b2\u0001\u0000\u0000\u0000\u00b4\u00b5" +
            "\u0001\u0000\u0000\u0000\u00b5\u00b9\u0001\u0000\u0000\u0000\u00b6\u00b4" +
            "\u0001\u0000\u0000\u0000\u00b7\u00b8\u0005*\u0000\u0000\u00b8\u00ba\u0003" +
            "4\u001a\u0000\u00b9\u00b7\u0001\u0000\u0000\u0000\u00b9\u00ba\u0001\u0000" +
            "\u0000\u0000\u00ba\u00bc\u0001\u0000\u0000\u0000\u00bb\u00bd\u0003\u0018" +
            "\f\u0000\u00bc\u00bb\u0001\u0000\u0000\u0000\u00bc\u00bd\u0001\u0000\u0000" +
            "\u0000\u00bd\u00bf\u0001\u0000\u0000\u0000\u00be\u00c0\u0003\u001c\u000e" +
            "\u0000\u00bf\u00be\u0001\u0000\u0000\u0000\u00bf\u00c0\u0001\u0000\u0000" +
            "\u0000\u00c0\u0017\u0001\u0000\u0000\u0000\u00c1\u00c2\u0005\u0015\u0000" +
            "\u0000\u00c2\u00c3\u0005\u0016\u0000\u0000\u00c3\u00c5\u0003B!\u0000\u00c4" +
            "\u00c6\u0003\u001a\r\u0000\u00c5\u00c4\u0001\u0000\u0000\u0000\u00c5\u00c6" +
            "\u0001\u0000\u0000\u0000\u00c6\u0019\u0001\u0000\u0000\u0000\u00c7\u00c8" +
            "\u0007\u0000\u0000\u0000\u00c8\u001b\u0001\u0000\u0000\u0000\u00c9\u00ca" +
            "\u0005\u0013\u0000\u0000\u00ca\u00d0\u0003 \u0010\u0000\u00cb\u00cc\u0005" +
            "\u0013\u0000\u0000\u00cc\u00cd\u0003 \u0010\u0000\u00cd\u00ce\u0003\u001e" +
            "\u000f\u0000\u00ce\u00d0\u0001\u0000\u0000\u0000\u00cf\u00c9\u0001\u0000" +
            "\u0000\u0000\u00cf\u00cb\u0001\u0000\u0000\u0000\u00d0\u001d\u0001\u0000" +
            "\u0000\u0000\u00d1\u00d2\u0005\u0014\u0000\u0000\u00d2\u00d3\u0003 \u0010" +
            "\u0000\u00d3\u001f\u0001\u0000\u0000\u0000\u00d4\u00d5\u00057\u0000\u0000" +
            "\u00d5!\u0001\u0000\u0000\u0000\u00d6\u00d9\u0003$\u0012\u0000\u00d7\u00d9" +
            "\u0003&\u0013\u0000\u00d8\u00d6\u0001\u0000\u0000\u0000\u00d8\u00d7\u0001" +
            "\u0000\u0000\u0000\u00d9#\u0001\u0000\u0000\u0000\u00da\u00dc\u00050\u0000" +
            "\u0000\u00db\u00da\u0001\u0000\u0000\u0000\u00db\u00dc\u0001\u0000\u0000" +
            "\u0000\u00dc\u00dd\u0001\u0000\u0000\u0000\u00dd\u00de\u0005/\u0000\u0000" +
            "\u00de\u00df\u0003>\u001f\u0000\u00df\u00e0\u0005-\u0000\u0000\u00e0\u00e1" +
            "\u0003@ \u0000\u00e1\u00e2\u0005\u000b\u0000\u0000\u00e2\u00e3\u0003@" +
            " \u0000\u00e3%\u0001\u0000\u0000\u0000\u00e4\u00e6\u0005.\u0000\u0000" +
            "\u00e5\u00e7\u00051\u0000\u0000\u00e6\u00e5\u0001\u0000\u0000\u0000\u00e6" +
            "\u00e7\u0001\u0000\u0000\u0000\u00e7\u00e8\u0001\u0000\u0000\u0000\u00e8" +
            "\u00e9\u0005/\u0000\u0000\u00e9\u00ea\u0003>\u001f\u0000\u00ea\u00eb\u0005" +
            "-\u0000\u0000\u00eb\u00ec\u0003@ \u0000\u00ec\u00ed\u0005\u000b\u0000" +
            "\u0000\u00ed\u00ee\u0003@ \u0000\u00ee\'\u0001\u0000\u0000\u0000\u00ef" +
            "\u00f0\u0005(\u0000\u0000\u00f0\u00f1\u0003>\u001f\u0000\u00f1\u00f2\u0005" +
            "&\u0000\u0000\u00f2\u00f3\u0003B!\u0000\u00f3\u00f4\u0005\u000b\u0000" +
            "\u0000\u00f4\u00fc\u0003:\u001d\u0000\u00f5\u00f6\u0005\u0002\u0000\u0000" +
            "\u00f6\u00f7\u0003B!\u0000\u00f7\u00f8\u0005\u000b\u0000\u0000\u00f8\u00f9" +
            "\u0003:\u001d\u0000\u00f9\u00fb\u0001\u0000\u0000\u0000\u00fa\u00f5\u0001" +
            "\u0000\u0000\u0000\u00fb\u00fe\u0001\u0000\u0000\u0000\u00fc\u00fa\u0001" +
            "\u0000\u0000\u0000\u00fc\u00fd\u0001\u0000\u0000\u0000\u00fd\u0101\u0001" +
            "\u0000\u0000\u0000\u00fe\u00fc\u0001\u0000\u0000\u0000\u00ff\u0100\u0005" +
            "*\u0000\u0000\u0100\u0102\u00034\u001a\u0000\u0101\u00ff\u0001\u0000\u0000" +
            "\u0000\u0101\u0102\u0001\u0000\u0000\u0000\u0102)\u0001\u0000\u0000\u0000" +
            "\u0103\u0104\u0003B!\u0000\u0104\u0106\u00032\u0019\u0000\u0105\u0107" +
            "\u0003,\u0016\u0000\u0106\u0105\u0001\u0000\u0000\u0000\u0106\u0107\u0001" +
            "\u0000\u0000\u0000\u0107+\u0001\u0000\u0000\u0000\u0108\u0109\u0005\u000e" +
            "\u0000\u0000\u0109\u010a\u0005\u000f\u0000\u0000\u010a-\u0001\u0000\u0000" +
            "\u0000\u010b\u010c\u0005\u0010\u0000\u0000\u010c\u010d\u0005\u0011\u0000" +
            "\u0000\u010d/\u0001\u0000\u0000\u0000\u010e\u010f\u0005\u0010\u0000\u0000" +
            "\u010f\u0110\u0005\u000e\u0000\u0000\u0110\u0111\u0005\u0011\u0000\u0000" +
            "\u01111\u0001\u0000\u0000\u0000\u0112\u0113\u0007\u0001\u0000\u0000\u0113" +
            "3\u0001\u0000\u0000\u0000\u0114\u0115\u0003B!\u0000\u0115\u0116\u0003" +
            "8\u001c\u0000\u0116\u0117\u0003:\u001d\u0000\u0117\u0126\u0001\u0000\u0000" +
            "\u0000\u0118\u0119\u0003B!\u0000\u0119\u011a\u00038\u001c\u0000\u011a" +
            "\u0122\u0003:\u001d\u0000\u011b\u011c\u00036\u001b\u0000\u011c\u011d\u0003" +
            "B!\u0000\u011d\u011e\u00038\u001c\u0000\u011e\u011f\u0003:\u001d\u0000" +
            "\u011f\u0121\u0001\u0000\u0000\u0000\u0120\u011b\u0001\u0000\u0000\u0000" +
            "\u0121\u0124\u0001\u0000\u0000\u0000\u0122\u0120\u0001\u0000\u0000\u0000" +
            "\u0122\u0123\u0001\u0000\u0000\u0000\u0123\u0126\u0001\u0000\u0000\u0000" +
            "\u0124\u0122\u0001\u0000\u0000\u0000\u0125\u0114\u0001\u0000\u0000\u0000" +
            "\u0125\u0118\u0001\u0000\u0000\u0000\u01265\u0001\u0000\u0000\u0000\u0127" +
            "\u0128\u0007\u0002\u0000\u0000\u01287\u0001\u0000\u0000\u0000\u0129\u012a" +
            "\u0007\u0003\u0000\u0000\u012a9\u0001\u0000\u0000\u0000\u012b\u012c\u0007" +
            "\u0004\u0000\u0000\u012c;\u0001\u0000\u0000\u0000\u012d\u012e\u0005\u0005" +
            "\u0000\u0000\u012e\u0133\u0003:\u001d\u0000\u012f\u0130\u0005\u0002\u0000" +
            "\u0000\u0130\u0132\u0003:\u001d\u0000\u0131\u012f\u0001\u0000\u0000\u0000" +
            "\u0132\u0135\u0001\u0000\u0000\u0000\u0133\u0131\u0001\u0000\u0000\u0000" +
            "\u0133\u0134\u0001\u0000\u0000\u0000\u0134\u0136\u0001\u0000\u0000\u0000" +
            "\u0135\u0133\u0001\u0000\u0000\u0000\u0136\u0137\u0005\u0006\u0000\u0000" +
            "\u0137=\u0001\u0000\u0000\u0000\u0138\u0139\u00056\u0000\u0000\u0139?" +
            "\u0001\u0000\u0000\u0000\u013a\u013b\u00056\u0000\u0000\u013b\u013c\u0005" +
            "\u0004\u0000\u0000\u013c\u013d\u00056\u0000\u0000\u013dA\u0001\u0000\u0000" +
            "\u0000\u013e\u0143\u00056\u0000\u0000\u013f\u0140\u00056\u0000\u0000\u0140" +
            "\u0141\u0005\u0004\u0000\u0000\u0141\u0143\u00056\u0000\u0000\u0142\u013e" +
            "\u0001\u0000\u0000\u0000\u0142\u013f\u0001\u0000\u0000\u0000\u0143C\u0001" +
            "\u0000\u0000\u0000\u001bFQZqz\u0084\u0089\u0096\u009b\u00a3\u00ac\u00b4" +
            "\u00b9\u00bc\u00bf\u00c5\u00cf\u00d8\u00db\u00e6\u00fc\u0101\u0106\u0122" +
            "\u0125\u0133\u0142";
    public static final ATN _ATN =
        new ATNDeserializer().deserialize(_serializedATN.toCharArray());

    static {
        _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
        for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
            _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
        }
    }
}