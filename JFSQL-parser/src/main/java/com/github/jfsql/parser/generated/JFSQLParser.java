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

    public static final int
        COL = 1, SCOL = 2, DOT = 3, OPEN_PAR = 4, CLOSE_PAR = 5, LT = 6, LT_EQ = 7, GT = 8, GT_EQ = 9,
        EQ = 10, NOT_EQ = 11, QUESTION_MARK = 12, NOT = 13, NULL = 14, IF = 15, EXISTS = 16,
        LIKE = 17, LIMIT = 18, OFFSET = 19, AND = 20, OR = 21, ADD = 22, ALTER = 23, CREATE = 24,
        COLUMN = 25, DELETE = 26, DATABASE = 27, DROP = 28, FROM = 29, INSERT = 30, INTO = 31,
        SELECT = 32, SET = 33, TABLE = 34, UPDATE = 35, VALUES = 36, WHERE = 37, RENAME = 38,
        TO = 39, ON = 40, LEFT = 41, JOIN = 42, INNER = 43, OUTER = 44, INTEGER = 45, REAL = 46,
        TEXT = 47, BLOB = 48, IDENTIFIER = 49, NUMERIC_LITERAL = 50, STRING_LITERAL = 51,
        SPACES = 52;
    public static final int
        RULE_root = 0, RULE_statement = 1, RULE_alterTable = 2, RULE_renameTable = 3,
        RULE_renameColumn = 4, RULE_addColumn = 5, RULE_dropColumn = 6, RULE_createTable = 7,
        RULE_createDatabase = 8, RULE_delete = 9, RULE_dropTable = 10, RULE_dropDatabase = 11,
        RULE_insert = 12, RULE_select = 13, RULE_limit = 14, RULE_offset = 15,
        RULE_numericValue = 16, RULE_joinOperation = 17, RULE_innerJoin = 18,
        RULE_leftJoin = 19, RULE_update = 20, RULE_columnDefinition = 21, RULE_notNull = 22,
        RULE_ifExists = 23, RULE_ifNotExists = 24, RULE_columnType = 25, RULE_expr = 26,
        RULE_binaryOperator = 27, RULE_symbol = 28, RULE_value = 29, RULE_valuesInParentheses = 30,
        RULE_databaseURL = 31, RULE_tableName = 32, RULE_tableDotColumnName = 33,
        RULE_columnName = 34;
    public static final String[] ruleNames = makeRuleNames();
    /**
     * @deprecated Use {@link #VOCABULARY} instead.
     */
    @Deprecated
    public static final String[] tokenNames;
    public static final String _serializedATN =
        "\u0004\u00014\u0148\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002" +
            "\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002" +
            "\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002" +
            "\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002" +
            "\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f" +
            "\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012" +
            "\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015" +
            "\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007\u0018" +
            "\u0002\u0019\u0007\u0019\u0002\u001a\u0007\u001a\u0002\u001b\u0007\u001b" +
            "\u0002\u001c\u0007\u001c\u0002\u001d\u0007\u001d\u0002\u001e\u0007\u001e" +
            "\u0002\u001f\u0007\u001f\u0002 \u0007 \u0002!\u0007!\u0002\"\u0007\"\u0001" +
            "\u0000\u0001\u0000\u0003\u0000I\b\u0000\u0001\u0000\u0001\u0000\u0001" +
            "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001" +
            "\u0001\u0001\u0001\u0001\u0001\u0003\u0001V\b\u0001\u0001\u0002\u0001" +
            "\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0003" +
            "\u0002_\b\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001" +
            "\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001" +
            "\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006\u0001" +
            "\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001\u0007\u0003\u0007v\b" +
            "\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0005" +
            "\u0007}\b\u0007\n\u0007\f\u0007\u0080\t\u0007\u0001\u0007\u0001\u0007" +
            "\u0001\b\u0001\b\u0001\b\u0001\b\u0001\t\u0001\t\u0001\t\u0001\t\u0001" +
            "\t\u0003\t\u008d\b\t\u0001\n\u0001\n\u0001\n\u0003\n\u0092\b\n\u0001\n" +
            "\u0001\n\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\f\u0001" +
            "\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0005\f\u00a1\b\f\n\f\f\f\u00a4" +
            "\t\f\u0001\f\u0001\f\u0003\f\u00a8\b\f\u0001\f\u0001\f\u0001\f\u0001\f" +
            "\u0005\f\u00ae\b\f\n\f\f\f\u00b1\t\f\u0001\r\u0001\r\u0001\r\u0001\r\u0005" +
            "\r\u00b7\b\r\n\r\f\r\u00ba\t\r\u0001\r\u0001\r\u0001\r\u0005\r\u00bf\b" +
            "\r\n\r\f\r\u00c2\t\r\u0001\r\u0001\r\u0003\r\u00c6\b\r\u0001\r\u0003\r" +
            "\u00c9\b\r\u0001\u000e\u0001\u000e\u0001\u000e\u0001\u000e\u0001\u000e" +
            "\u0001\u000e\u0003\u000e\u00d1\b\u000e\u0001\u000f\u0001\u000f\u0001\u000f" +
            "\u0001\u0010\u0001\u0010\u0001\u0011\u0001\u0011\u0003\u0011\u00da\b\u0011" +
            "\u0001\u0012\u0003\u0012\u00dd\b\u0012\u0001\u0012\u0001\u0012\u0001\u0012" +
            "\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0013\u0001\u0013" +
            "\u0003\u0013\u00e8\b\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013" +
            "\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0014\u0001\u0014\u0001\u0014" +
            "\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014" +
            "\u0001\u0014\u0001\u0014\u0005\u0014\u00fc\b\u0014\n\u0014\f\u0014\u00ff" +
            "\t\u0014\u0001\u0014\u0001\u0014\u0003\u0014\u0103\b\u0014\u0001\u0015" +
            "\u0001\u0015\u0001\u0015\u0003\u0015\u0108\b\u0015\u0001\u0016\u0001\u0016" +
            "\u0001\u0016\u0001\u0017\u0001\u0017\u0001\u0017\u0001\u0018\u0001\u0018" +
            "\u0001\u0018\u0001\u0018\u0001\u0019\u0001\u0019\u0001\u001a\u0001\u001a" +
            "\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a" +
            "\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0005\u001a\u0122\b\u001a" +
            "\n\u001a\f\u001a\u0125\t\u001a\u0003\u001a\u0127\b\u001a\u0001\u001b\u0001" +
            "\u001b\u0001\u001c\u0001\u001c\u0001\u001d\u0001\u001d\u0001\u001e\u0001" +
            "\u001e\u0001\u001e\u0001\u001e\u0005\u001e\u0133\b\u001e\n\u001e\f\u001e" +
            "\u0136\t\u001e\u0001\u001e\u0001\u001e\u0001\u001f\u0001\u001f\u0001 " +
            "\u0001 \u0001!\u0001!\u0001!\u0001!\u0001\"\u0001\"\u0001\"\u0001\"\u0003" +
            "\"\u0146\b\"\u0001\"\u0000\u0000#\u0000\u0002\u0004\u0006\b\n\f\u000e" +
            "\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e \"$&(*,.02468:<>@BD\u0000" +
            "\u0004\u0001\u0000-0\u0001\u0000\u0014\u0015\u0002\u0000\u0006\u000b\u0011" +
            "\u0011\u0002\u0000\f\f23\u0146\u0000F\u0001\u0000\u0000\u0000\u0002U\u0001" +
            "\u0000\u0000\u0000\u0004W\u0001\u0000\u0000\u0000\u0006`\u0001\u0000\u0000" +
            "\u0000\bd\u0001\u0000\u0000\u0000\nj\u0001\u0000\u0000\u0000\fn\u0001" +
            "\u0000\u0000\u0000\u000er\u0001\u0000\u0000\u0000\u0010\u0083\u0001\u0000" +
            "\u0000\u0000\u0012\u0087\u0001\u0000\u0000\u0000\u0014\u008e\u0001\u0000" +
            "\u0000\u0000\u0016\u0095\u0001\u0000\u0000\u0000\u0018\u0099\u0001\u0000" +
            "\u0000\u0000\u001a\u00b2\u0001\u0000\u0000\u0000\u001c\u00d0\u0001\u0000" +
            "\u0000\u0000\u001e\u00d2\u0001\u0000\u0000\u0000 \u00d5\u0001\u0000\u0000" +
            "\u0000\"\u00d9\u0001\u0000\u0000\u0000$\u00dc\u0001\u0000\u0000\u0000" +
            "&\u00e5\u0001\u0000\u0000\u0000(\u00f0\u0001\u0000\u0000\u0000*\u0104" +
            "\u0001\u0000\u0000\u0000,\u0109\u0001\u0000\u0000\u0000.\u010c\u0001\u0000" +
            "\u0000\u00000\u010f\u0001\u0000\u0000\u00002\u0113\u0001\u0000\u0000\u0000" +
            "4\u0126\u0001\u0000\u0000\u00006\u0128\u0001\u0000\u0000\u00008\u012a" +
            "\u0001\u0000\u0000\u0000:\u012c\u0001\u0000\u0000\u0000<\u012e\u0001\u0000" +
            "\u0000\u0000>\u0139\u0001\u0000\u0000\u0000@\u013b\u0001\u0000\u0000\u0000" +
            "B\u013d\u0001\u0000\u0000\u0000D\u0145\u0001\u0000\u0000\u0000FH\u0003" +
            "\u0002\u0001\u0000GI\u0005\u0002\u0000\u0000HG\u0001\u0000\u0000\u0000" +
            "HI\u0001\u0000\u0000\u0000IJ\u0001\u0000\u0000\u0000JK\u0005\u0000\u0000" +
            "\u0001K\u0001\u0001\u0000\u0000\u0000LV\u0003\u0004\u0002\u0000MV\u0003" +
            "\u000e\u0007\u0000NV\u0003\u0010\b\u0000OV\u0003\u0012\t\u0000PV\u0003" +
            "\u0014\n\u0000QV\u0003\u0016\u000b\u0000RV\u0003\u0018\f\u0000SV\u0003" +
            "\u001a\r\u0000TV\u0003(\u0014\u0000UL\u0001\u0000\u0000\u0000UM\u0001" +
            "\u0000\u0000\u0000UN\u0001\u0000\u0000\u0000UO\u0001\u0000\u0000\u0000" +
            "UP\u0001\u0000\u0000\u0000UQ\u0001\u0000\u0000\u0000UR\u0001\u0000\u0000" +
            "\u0000US\u0001\u0000\u0000\u0000UT\u0001\u0000\u0000\u0000V\u0003\u0001" +
            "\u0000\u0000\u0000WX\u0005\u0017\u0000\u0000XY\u0005\"\u0000\u0000Y^\u0003" +
            "@ \u0000Z_\u0003\u0006\u0003\u0000[_\u0003\b\u0004\u0000\\_\u0003\n\u0005" +
            "\u0000]_\u0003\f\u0006\u0000^Z\u0001\u0000\u0000\u0000^[\u0001\u0000\u0000" +
            "\u0000^\\\u0001\u0000\u0000\u0000^]\u0001\u0000\u0000\u0000_\u0005\u0001" +
            "\u0000\u0000\u0000`a\u0005&\u0000\u0000ab\u0005\'\u0000\u0000bc\u0003" +
            "@ \u0000c\u0007\u0001\u0000\u0000\u0000de\u0005&\u0000\u0000ef\u0005\u0019" +
            "\u0000\u0000fg\u0003D\"\u0000gh\u0005\'\u0000\u0000hi\u0003D\"\u0000i" +
            "\t\u0001\u0000\u0000\u0000jk\u0005\u0016\u0000\u0000kl\u0005\u0019\u0000" +
            "\u0000lm\u0003*\u0015\u0000m\u000b\u0001\u0000\u0000\u0000no\u0005\u001c" +
            "\u0000\u0000op\u0005\u0019\u0000\u0000pq\u0003D\"\u0000q\r\u0001\u0000" +
            "\u0000\u0000rs\u0005\u0018\u0000\u0000su\u0005\"\u0000\u0000tv\u00030" +
            "\u0018\u0000ut\u0001\u0000\u0000\u0000uv\u0001\u0000\u0000\u0000vw\u0001" +
            "\u0000\u0000\u0000wx\u0003@ \u0000xy\u0005\u0004\u0000\u0000y~\u0003*" +
            "\u0015\u0000z{\u0005\u0001\u0000\u0000{}\u0003*\u0015\u0000|z\u0001\u0000" +
            "\u0000\u0000}\u0080\u0001\u0000\u0000\u0000~|\u0001\u0000\u0000\u0000" +
            "~\u007f\u0001\u0000\u0000\u0000\u007f\u0081\u0001\u0000\u0000\u0000\u0080" +
            "~\u0001\u0000\u0000\u0000\u0081\u0082\u0005\u0005\u0000\u0000\u0082\u000f" +
            "\u0001\u0000\u0000\u0000\u0083\u0084\u0005\u0018\u0000\u0000\u0084\u0085" +
            "\u0005\u001b\u0000\u0000\u0085\u0086\u0003>\u001f\u0000\u0086\u0011\u0001" +
            "\u0000\u0000\u0000\u0087\u0088\u0005\u001a\u0000\u0000\u0088\u0089\u0005" +
            "\u001d\u0000\u0000\u0089\u008c\u0003@ \u0000\u008a\u008b\u0005%\u0000" +
            "\u0000\u008b\u008d\u00034\u001a\u0000\u008c\u008a\u0001\u0000\u0000\u0000" +
            "\u008c\u008d\u0001\u0000\u0000\u0000\u008d\u0013\u0001\u0000\u0000\u0000" +
            "\u008e\u008f\u0005\u001c\u0000\u0000\u008f\u0091\u0005\"\u0000\u0000\u0090" +
            "\u0092\u0003.\u0017\u0000\u0091\u0090\u0001\u0000\u0000\u0000\u0091\u0092" +
            "\u0001\u0000\u0000\u0000\u0092\u0093\u0001\u0000\u0000\u0000\u0093\u0094" +
            "\u0003@ \u0000\u0094\u0015\u0001\u0000\u0000\u0000\u0095\u0096\u0005\u001c" +
            "\u0000\u0000\u0096\u0097\u0005\u001b\u0000\u0000\u0097\u0098\u0003>\u001f" +
            "\u0000\u0098\u0017\u0001\u0000\u0000\u0000\u0099\u009a\u0005\u001e\u0000" +
            "\u0000\u009a\u009b\u0005\u001f\u0000\u0000\u009b\u00a7\u0003@ \u0000\u009c" +
            "\u009d\u0005\u0004\u0000\u0000\u009d\u00a2\u0003D\"\u0000\u009e\u009f" +
            "\u0005\u0001\u0000\u0000\u009f\u00a1\u0003D\"\u0000\u00a0\u009e\u0001" +
            "\u0000\u0000\u0000\u00a1\u00a4\u0001\u0000\u0000\u0000\u00a2\u00a0\u0001" +
            "\u0000\u0000\u0000\u00a2\u00a3\u0001\u0000\u0000\u0000\u00a3\u00a5\u0001" +
            "\u0000\u0000\u0000\u00a4\u00a2\u0001\u0000\u0000\u0000\u00a5\u00a6\u0005" +
            "\u0005\u0000\u0000\u00a6\u00a8\u0001\u0000\u0000\u0000\u00a7\u009c\u0001" +
            "\u0000\u0000\u0000\u00a7\u00a8\u0001\u0000\u0000\u0000\u00a8\u00a9\u0001" +
            "\u0000\u0000\u0000\u00a9\u00aa\u0005$\u0000\u0000\u00aa\u00af\u0003<\u001e" +
            "\u0000\u00ab\u00ac\u0005\u0001\u0000\u0000\u00ac\u00ae\u0003<\u001e\u0000" +
            "\u00ad\u00ab\u0001\u0000\u0000\u0000\u00ae\u00b1\u0001\u0000\u0000\u0000" +
            "\u00af\u00ad\u0001\u0000\u0000\u0000\u00af\u00b0\u0001\u0000\u0000\u0000" +
            "\u00b0\u0019\u0001\u0000\u0000\u0000\u00b1\u00af\u0001\u0000\u0000\u0000" +
            "\u00b2\u00b3\u0005 \u0000\u0000\u00b3\u00b8\u0003D\"\u0000\u00b4\u00b5" +
            "\u0005\u0001\u0000\u0000\u00b5\u00b7\u0003D\"\u0000\u00b6\u00b4\u0001" +
            "\u0000\u0000\u0000\u00b7\u00ba\u0001\u0000\u0000\u0000\u00b8\u00b6\u0001" +
            "\u0000\u0000\u0000\u00b8\u00b9\u0001\u0000\u0000\u0000\u00b9\u00bb\u0001" +
            "\u0000\u0000\u0000\u00ba\u00b8\u0001\u0000\u0000\u0000\u00bb\u00bc\u0005" +
            "\u001d\u0000\u0000\u00bc\u00c0\u0003@ \u0000\u00bd\u00bf\u0003\"\u0011" +
            "\u0000\u00be\u00bd\u0001\u0000\u0000\u0000\u00bf\u00c2\u0001\u0000\u0000" +
            "\u0000\u00c0\u00be\u0001\u0000\u0000\u0000\u00c0\u00c1\u0001\u0000\u0000" +
            "\u0000\u00c1\u00c5\u0001\u0000\u0000\u0000\u00c2\u00c0\u0001\u0000\u0000" +
            "\u0000\u00c3\u00c4\u0005%\u0000\u0000\u00c4\u00c6\u00034\u001a\u0000\u00c5" +
            "\u00c3\u0001\u0000\u0000\u0000\u00c5\u00c6\u0001\u0000\u0000\u0000\u00c6" +
            "\u00c8\u0001\u0000\u0000\u0000\u00c7\u00c9\u0003\u001c\u000e\u0000\u00c8" +
            "\u00c7\u0001\u0000\u0000\u0000\u00c8\u00c9\u0001\u0000\u0000\u0000\u00c9" +
            "\u001b\u0001\u0000\u0000\u0000\u00ca\u00cb\u0005\u0012\u0000\u0000\u00cb" +
            "\u00d1\u0003 \u0010\u0000\u00cc\u00cd\u0005\u0012\u0000\u0000\u00cd\u00ce" +
            "\u0003 \u0010\u0000\u00ce\u00cf\u0003\u001e\u000f\u0000\u00cf\u00d1\u0001" +
            "\u0000\u0000\u0000\u00d0\u00ca\u0001\u0000\u0000\u0000\u00d0\u00cc\u0001" +
            "\u0000\u0000\u0000\u00d1\u001d\u0001\u0000\u0000\u0000\u00d2\u00d3\u0005" +
            "\u0013\u0000\u0000\u00d3\u00d4\u0003 \u0010\u0000\u00d4\u001f\u0001\u0000" +
            "\u0000\u0000\u00d5\u00d6\u00052\u0000\u0000\u00d6!\u0001\u0000\u0000\u0000" +
            "\u00d7\u00da\u0003$\u0012\u0000\u00d8\u00da\u0003&\u0013\u0000\u00d9\u00d7" +
            "\u0001\u0000\u0000\u0000\u00d9\u00d8\u0001\u0000\u0000\u0000\u00da#\u0001" +
            "\u0000\u0000\u0000\u00db\u00dd\u0005+\u0000\u0000\u00dc\u00db\u0001\u0000" +
            "\u0000\u0000\u00dc\u00dd\u0001\u0000\u0000\u0000\u00dd\u00de\u0001\u0000" +
            "\u0000\u0000\u00de\u00df\u0005*\u0000\u0000\u00df\u00e0\u0003@ \u0000" +
            "\u00e0\u00e1\u0005(\u0000\u0000\u00e1\u00e2\u0003B!\u0000\u00e2\u00e3" +
            "\u0005\n\u0000\u0000\u00e3\u00e4\u0003B!\u0000\u00e4%\u0001\u0000\u0000" +
            "\u0000\u00e5\u00e7\u0005)\u0000\u0000\u00e6\u00e8\u0005,\u0000\u0000\u00e7" +
            "\u00e6\u0001\u0000\u0000\u0000\u00e7\u00e8\u0001\u0000\u0000\u0000\u00e8" +
            "\u00e9\u0001\u0000\u0000\u0000\u00e9\u00ea\u0005*\u0000\u0000\u00ea\u00eb" +
            "\u0003@ \u0000\u00eb\u00ec\u0005(\u0000\u0000\u00ec\u00ed\u0003B!\u0000" +
            "\u00ed\u00ee\u0005\n\u0000\u0000\u00ee\u00ef\u0003B!\u0000\u00ef\'\u0001" +
            "\u0000\u0000\u0000\u00f0\u00f1\u0005#\u0000\u0000\u00f1\u00f2\u0003@ " +
            "\u0000\u00f2\u00f3\u0005!\u0000\u0000\u00f3\u00f4\u0003D\"\u0000\u00f4" +
            "\u00f5\u0005\n\u0000\u0000\u00f5\u00fd\u0003:\u001d\u0000\u00f6\u00f7" +
            "\u0005\u0001\u0000\u0000\u00f7\u00f8\u0003D\"\u0000\u00f8\u00f9\u0005" +
            "\n\u0000\u0000\u00f9\u00fa\u0003:\u001d\u0000\u00fa\u00fc\u0001\u0000" +
            "\u0000\u0000\u00fb\u00f6\u0001\u0000\u0000\u0000\u00fc\u00ff\u0001\u0000" +
            "\u0000\u0000\u00fd\u00fb\u0001\u0000\u0000\u0000\u00fd\u00fe\u0001\u0000" +
            "\u0000\u0000\u00fe\u0102\u0001\u0000\u0000\u0000\u00ff\u00fd\u0001\u0000" +
            "\u0000\u0000\u0100\u0101\u0005%\u0000\u0000\u0101\u0103\u00034\u001a\u0000" +
            "\u0102\u0100\u0001\u0000\u0000\u0000\u0102\u0103\u0001\u0000\u0000\u0000" +
            "\u0103)\u0001\u0000\u0000\u0000\u0104\u0105\u0003D\"\u0000\u0105\u0107" +
            "\u00032\u0019\u0000\u0106\u0108\u0003,\u0016\u0000\u0107\u0106\u0001\u0000" +
            "\u0000\u0000\u0107\u0108\u0001\u0000\u0000\u0000\u0108+\u0001\u0000\u0000" +
            "\u0000\u0109\u010a\u0005\r\u0000\u0000\u010a\u010b\u0005\u000e\u0000\u0000" +
            "\u010b-\u0001\u0000\u0000\u0000\u010c\u010d\u0005\u000f\u0000\u0000\u010d" +
            "\u010e\u0005\u0010\u0000\u0000\u010e/\u0001\u0000\u0000\u0000\u010f\u0110" +
            "\u0005\u000f\u0000\u0000\u0110\u0111\u0005\r\u0000\u0000\u0111\u0112\u0005" +
            "\u0010\u0000\u0000\u01121\u0001\u0000\u0000\u0000\u0113\u0114\u0007\u0000" +
            "\u0000\u0000\u01143\u0001\u0000\u0000\u0000\u0115\u0116\u0003D\"\u0000" +
            "\u0116\u0117\u00038\u001c\u0000\u0117\u0118\u0003:\u001d\u0000\u0118\u0127" +
            "\u0001\u0000\u0000\u0000\u0119\u011a\u0003D\"\u0000\u011a\u011b\u0003" +
            "8\u001c\u0000\u011b\u0123\u0003:\u001d\u0000\u011c\u011d\u00036\u001b" +
            "\u0000\u011d\u011e\u0003D\"\u0000\u011e\u011f\u00038\u001c\u0000\u011f" +
            "\u0120\u0003:\u001d\u0000\u0120\u0122\u0001\u0000\u0000\u0000\u0121\u011c" +
            "\u0001\u0000\u0000\u0000\u0122\u0125\u0001\u0000\u0000\u0000\u0123\u0121" +
            "\u0001\u0000\u0000\u0000\u0123\u0124\u0001\u0000\u0000\u0000\u0124\u0127" +
            "\u0001\u0000\u0000\u0000\u0125\u0123\u0001\u0000\u0000\u0000\u0126\u0115" +
            "\u0001\u0000\u0000\u0000\u0126\u0119\u0001\u0000\u0000\u0000\u01275\u0001" +
            "\u0000\u0000\u0000\u0128\u0129\u0007\u0001\u0000\u0000\u01297\u0001\u0000" +
            "\u0000\u0000\u012a\u012b\u0007\u0002\u0000\u0000\u012b9\u0001\u0000\u0000" +
            "\u0000\u012c\u012d\u0007\u0003\u0000\u0000\u012d;\u0001\u0000\u0000\u0000" +
            "\u012e\u012f\u0005\u0004\u0000\u0000\u012f\u0134\u0003:\u001d\u0000\u0130" +
            "\u0131\u0005\u0001\u0000\u0000\u0131\u0133\u0003:\u001d\u0000\u0132\u0130" +
            "\u0001\u0000\u0000\u0000\u0133\u0136\u0001\u0000\u0000\u0000\u0134\u0132" +
            "\u0001\u0000\u0000\u0000\u0134\u0135\u0001\u0000\u0000\u0000\u0135\u0137" +
            "\u0001\u0000\u0000\u0000\u0136\u0134\u0001\u0000\u0000\u0000\u0137\u0138" +
            "\u0005\u0005\u0000\u0000\u0138=\u0001\u0000\u0000\u0000\u0139\u013a\u0005" +
            "1\u0000\u0000\u013a?\u0001\u0000\u0000\u0000\u013b\u013c\u00051\u0000" +
            "\u0000\u013cA\u0001\u0000\u0000\u0000\u013d\u013e\u00051\u0000\u0000\u013e" +
            "\u013f\u0005\u0003\u0000\u0000\u013f\u0140\u00051\u0000\u0000\u0140C\u0001" +
            "\u0000\u0000\u0000\u0141\u0146\u00051\u0000\u0000\u0142\u0143\u00051\u0000" +
            "\u0000\u0143\u0144\u0005\u0003\u0000\u0000\u0144\u0146\u00051\u0000\u0000" +
            "\u0145\u0141\u0001\u0000\u0000\u0000\u0145\u0142\u0001\u0000\u0000\u0000" +
            "\u0146E\u0001\u0000\u0000\u0000\u0019HU^u~\u008c\u0091\u00a2\u00a7\u00af" +
            "\u00b8\u00c0\u00c5\u00c8\u00d0\u00d9\u00dc\u00e7\u00fd\u0102\u0107\u0123" +
            "\u0126\u0134\u0145";
    public static final ATN _ATN =
        new ATNDeserializer().deserialize(_serializedATN.toCharArray());
    protected static final DFA[] _decisionToDFA;
    protected static final PredictionContextCache _sharedContextCache =
        new PredictionContextCache();
    private static final String[] _LITERAL_NAMES = makeLiteralNames();
    private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
    public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

    static {
        RuntimeMetaData.checkVersion("4.12.0", RuntimeMetaData.VERSION);
    }

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

    static {
        _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
        for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
            _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
        }
    }

    public JFSQLParser(TokenStream input) {
        super(input);
        _interp = new ParserATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
    }

    private static String[] makeRuleNames() {
        return new String[]{
            "root", "statement", "alterTable", "renameTable", "renameColumn", "addColumn",
            "dropColumn", "createTable", "createDatabase", "delete", "dropTable",
            "dropDatabase", "insert", "select", "limit", "offset", "numericValue",
            "joinOperation", "innerJoin", "leftJoin", "update", "columnDefinition",
            "notNull", "ifExists", "ifNotExists", "columnType", "expr", "binaryOperator",
            "symbol", "value", "valuesInParentheses", "databaseURL", "tableName",
            "tableDotColumnName", "columnName"
        };
    }

    private static String[] makeLiteralNames() {
        return new String[]{
            null, "','", "';'", "'.'", "'('", "')'", "'<'", "'<='", "'>'", "'>='",
            "'='", "'!='", "'?'"
        };
    }

    private static String[] makeSymbolicNames() {
        return new String[]{
            null, "COL", "SCOL", "DOT", "OPEN_PAR", "CLOSE_PAR", "LT", "LT_EQ", "GT",
            "GT_EQ", "EQ", "NOT_EQ", "QUESTION_MARK", "NOT", "NULL", "IF", "EXISTS",
            "LIKE", "LIMIT", "OFFSET", "AND", "OR", "ADD", "ALTER", "CREATE", "COLUMN",
            "DELETE", "DATABASE", "DROP", "FROM", "INSERT", "INTO", "SELECT", "SET",
            "TABLE", "UPDATE", "VALUES", "WHERE", "RENAME", "TO", "ON", "LEFT", "JOIN",
            "INNER", "OUTER", "INTEGER", "REAL", "TEXT", "BLOB", "IDENTIFIER", "NUMERIC_LITERAL",
            "STRING_LITERAL", "SPACES"
        };
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

    public final RootContext root() throws RecognitionException {
        RootContext _localctx = new RootContext(_ctx, getState());
        enterRule(_localctx, 0, RULE_root);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(70);
                statement();
                setState(72);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == SCOL) {
                    {
                        setState(71);
                        match(SCOL);
                    }
                }

                setState(74);
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

    public final StatementContext statement() throws RecognitionException {
        StatementContext _localctx = new StatementContext(_ctx, getState());
        enterRule(_localctx, 2, RULE_statement);
        try {
            setState(85);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 1, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(76);
                    alterTable();
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(77);
                    createTable();
                }
                break;
                case 3:
                    enterOuterAlt(_localctx, 3);
                {
                    setState(78);
                    createDatabase();
                }
                break;
                case 4:
                    enterOuterAlt(_localctx, 4);
                {
                    setState(79);
                    delete();
                }
                break;
                case 5:
                    enterOuterAlt(_localctx, 5);
                {
                    setState(80);
                    dropTable();
                }
                break;
                case 6:
                    enterOuterAlt(_localctx, 6);
                {
                    setState(81);
                    dropDatabase();
                }
                break;
                case 7:
                    enterOuterAlt(_localctx, 7);
                {
                    setState(82);
                    insert();
                }
                break;
                case 8:
                    enterOuterAlt(_localctx, 8);
                {
                    setState(83);
                    select();
                }
                break;
                case 9:
                    enterOuterAlt(_localctx, 9);
                {
                    setState(84);
                    update();
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

    public final AlterTableContext alterTable() throws RecognitionException {
        AlterTableContext _localctx = new AlterTableContext(_ctx, getState());
        enterRule(_localctx, 4, RULE_alterTable);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(87);
                match(ALTER);
                setState(88);
                match(TABLE);
                setState(89);
                tableName();
                setState(94);
                _errHandler.sync(this);
                switch (getInterpreter().adaptivePredict(_input, 2, _ctx)) {
                    case 1: {
                        setState(90);
                        renameTable();
                    }
                    break;
                    case 2: {
                        setState(91);
                        renameColumn();
                    }
                    break;
                    case 3: {
                        setState(92);
                        addColumn();
                    }
                    break;
                    case 4: {
                        setState(93);
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

    public final RenameTableContext renameTable() throws RecognitionException {
        RenameTableContext _localctx = new RenameTableContext(_ctx, getState());
        enterRule(_localctx, 6, RULE_renameTable);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(96);
                match(RENAME);
                setState(97);
                match(TO);
                setState(98);
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

    public final RenameColumnContext renameColumn() throws RecognitionException {
        RenameColumnContext _localctx = new RenameColumnContext(_ctx, getState());
        enterRule(_localctx, 8, RULE_renameColumn);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(100);
                match(RENAME);
                setState(101);
                match(COLUMN);
                setState(102);
                columnName();
                setState(103);
                match(TO);
                setState(104);
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

    public final AddColumnContext addColumn() throws RecognitionException {
        AddColumnContext _localctx = new AddColumnContext(_ctx, getState());
        enterRule(_localctx, 10, RULE_addColumn);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(106);
                match(ADD);
                setState(107);
                match(COLUMN);
                setState(108);
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

    public final DropColumnContext dropColumn() throws RecognitionException {
        DropColumnContext _localctx = new DropColumnContext(_ctx, getState());
        enterRule(_localctx, 12, RULE_dropColumn);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(110);
                match(DROP);
                setState(111);
                match(COLUMN);
                setState(112);
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

    public final CreateTableContext createTable() throws RecognitionException {
        CreateTableContext _localctx = new CreateTableContext(_ctx, getState());
        enterRule(_localctx, 14, RULE_createTable);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(114);
                match(CREATE);
                setState(115);
                match(TABLE);
                setState(117);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == IF) {
                    {
                        setState(116);
                        ifNotExists();
                    }
                }

                setState(119);
                tableName();
                setState(120);
                match(OPEN_PAR);
                setState(121);
                columnDefinition();
                setState(126);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(122);
                            match(COL);
                            setState(123);
                            columnDefinition();
                        }
                    }
                    setState(128);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(129);
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

    public final CreateDatabaseContext createDatabase() throws RecognitionException {
        CreateDatabaseContext _localctx = new CreateDatabaseContext(_ctx, getState());
        enterRule(_localctx, 16, RULE_createDatabase);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(131);
                match(CREATE);
                setState(132);
                match(DATABASE);
                setState(133);
                databaseURL();
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

    public final DeleteContext delete() throws RecognitionException {
        DeleteContext _localctx = new DeleteContext(_ctx, getState());
        enterRule(_localctx, 18, RULE_delete);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(135);
                match(DELETE);
                setState(136);
                match(FROM);
                setState(137);
                tableName();
                setState(140);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == WHERE) {
                    {
                        setState(138);
                        match(WHERE);
                        setState(139);
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

    public final DropTableContext dropTable() throws RecognitionException {
        DropTableContext _localctx = new DropTableContext(_ctx, getState());
        enterRule(_localctx, 20, RULE_dropTable);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(142);
                match(DROP);
                setState(143);
                match(TABLE);
                setState(145);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == IF) {
                    {
                        setState(144);
                        ifExists();
                    }
                }

                setState(147);
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

    public final DropDatabaseContext dropDatabase() throws RecognitionException {
        DropDatabaseContext _localctx = new DropDatabaseContext(_ctx, getState());
        enterRule(_localctx, 22, RULE_dropDatabase);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(149);
                match(DROP);
                setState(150);
                match(DATABASE);
                setState(151);
                databaseURL();
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

    public final InsertContext insert() throws RecognitionException {
        InsertContext _localctx = new InsertContext(_ctx, getState());
        enterRule(_localctx, 24, RULE_insert);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(153);
                match(INSERT);
                setState(154);
                match(INTO);
                setState(155);
                tableName();
                setState(167);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == OPEN_PAR) {
                    {
                        setState(156);
                        match(OPEN_PAR);
                        setState(157);
                        columnName();
                        setState(162);
                        _errHandler.sync(this);
                        _la = _input.LA(1);
                        while (_la == COL) {
                            {
                                {
                                    setState(158);
                                    match(COL);
                                    setState(159);
                                    columnName();
                                }
                            }
                            setState(164);
                            _errHandler.sync(this);
                            _la = _input.LA(1);
                        }
                        setState(165);
                        match(CLOSE_PAR);
                    }
                }

                setState(169);
                match(VALUES);
                setState(170);
                valuesInParentheses();
                setState(175);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(171);
                            match(COL);
                            setState(172);
                            valuesInParentheses();
                        }
                    }
                    setState(177);
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

    public final SelectContext select() throws RecognitionException {
        SelectContext _localctx = new SelectContext(_ctx, getState());
        enterRule(_localctx, 26, RULE_select);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(178);
                match(SELECT);
                setState(179);
                columnName();
                setState(184);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(180);
                            match(COL);
                            setState(181);
                            columnName();
                        }
                    }
                    setState(186);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(187);
                match(FROM);
                setState(188);
                tableName();
                setState(192);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 15393162788864L) != 0)) {
                    {
                        {
                            setState(189);
                            joinOperation();
                        }
                    }
                    setState(194);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(197);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == WHERE) {
                    {
                        setState(195);
                        match(WHERE);
                        setState(196);
                        expr();
                    }
                }

                setState(200);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == LIMIT) {
                    {
                        setState(199);
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

    public final LimitContext limit() throws RecognitionException {
        LimitContext _localctx = new LimitContext(_ctx, getState());
        enterRule(_localctx, 28, RULE_limit);
        try {
            setState(208);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 14, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(202);
                    match(LIMIT);
                    setState(203);
                    numericValue();
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(204);
                    match(LIMIT);
                    setState(205);
                    numericValue();
                    setState(206);
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

    public final OffsetContext offset() throws RecognitionException {
        OffsetContext _localctx = new OffsetContext(_ctx, getState());
        enterRule(_localctx, 30, RULE_offset);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(210);
                match(OFFSET);
                setState(211);
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

    public final NumericValueContext numericValue() throws RecognitionException {
        NumericValueContext _localctx = new NumericValueContext(_ctx, getState());
        enterRule(_localctx, 32, RULE_numericValue);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(213);
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

    public final JoinOperationContext joinOperation() throws RecognitionException {
        JoinOperationContext _localctx = new JoinOperationContext(_ctx, getState());
        enterRule(_localctx, 34, RULE_joinOperation);
        try {
            setState(217);
            _errHandler.sync(this);
            switch (_input.LA(1)) {
                case JOIN:
                case INNER:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(215);
                    innerJoin();
                }
                break;
                case LEFT:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(216);
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

    public final InnerJoinContext innerJoin() throws RecognitionException {
        InnerJoinContext _localctx = new InnerJoinContext(_ctx, getState());
        enterRule(_localctx, 36, RULE_innerJoin);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(220);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == INNER) {
                    {
                        setState(219);
                        match(INNER);
                    }
                }

                setState(222);
                match(JOIN);
                setState(223);
                tableName();
                setState(224);
                match(ON);
                setState(225);
                tableDotColumnName();
                setState(226);
                match(EQ);
                setState(227);
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

    public final LeftJoinContext leftJoin() throws RecognitionException {
        LeftJoinContext _localctx = new LeftJoinContext(_ctx, getState());
        enterRule(_localctx, 38, RULE_leftJoin);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(229);
                match(LEFT);
                setState(231);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == OUTER) {
                    {
                        setState(230);
                        match(OUTER);
                    }
                }

                setState(233);
                match(JOIN);
                setState(234);
                tableName();
                setState(235);
                match(ON);
                setState(236);
                tableDotColumnName();
                setState(237);
                match(EQ);
                setState(238);
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

    public final UpdateContext update() throws RecognitionException {
        UpdateContext _localctx = new UpdateContext(_ctx, getState());
        enterRule(_localctx, 40, RULE_update);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(240);
                match(UPDATE);
                setState(241);
                tableName();
                setState(242);
                match(SET);
                setState(243);
                columnName();
                setState(244);
                match(EQ);
                setState(245);
                value();
                setState(253);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(246);
                            match(COL);
                            setState(247);
                            columnName();
                            setState(248);
                            match(EQ);
                            setState(249);
                            value();
                        }
                    }
                    setState(255);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(258);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == WHERE) {
                    {
                        setState(256);
                        match(WHERE);
                        setState(257);
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

    public final ColumnDefinitionContext columnDefinition() throws RecognitionException {
        ColumnDefinitionContext _localctx = new ColumnDefinitionContext(_ctx, getState());
        enterRule(_localctx, 42, RULE_columnDefinition);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(260);
                columnName();
                setState(261);
                columnType();
                setState(263);
                _errHandler.sync(this);
                _la = _input.LA(1);
                if (_la == NOT) {
                    {
                        setState(262);
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

    public final NotNullContext notNull() throws RecognitionException {
        NotNullContext _localctx = new NotNullContext(_ctx, getState());
        enterRule(_localctx, 44, RULE_notNull);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(265);
                match(NOT);
                setState(266);
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

    public final IfExistsContext ifExists() throws RecognitionException {
        IfExistsContext _localctx = new IfExistsContext(_ctx, getState());
        enterRule(_localctx, 46, RULE_ifExists);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(268);
                match(IF);
                setState(269);
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

    public final IfNotExistsContext ifNotExists() throws RecognitionException {
        IfNotExistsContext _localctx = new IfNotExistsContext(_ctx, getState());
        enterRule(_localctx, 48, RULE_ifNotExists);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(271);
                match(IF);
                setState(272);
                match(NOT);
                setState(273);
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

    public final ColumnTypeContext columnType() throws RecognitionException {
        ColumnTypeContext _localctx = new ColumnTypeContext(_ctx, getState());
        enterRule(_localctx, 50, RULE_columnType);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(275);
                _la = _input.LA(1);
                if (!((((_la) & ~0x3f) == 0 && ((1L << _la) & 527765581332480L) != 0))) {
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

    public final ExprContext expr() throws RecognitionException {
        ExprContext _localctx = new ExprContext(_ctx, getState());
        enterRule(_localctx, 52, RULE_expr);
        int _la;
        try {
            setState(294);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 22, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(277);
                    columnName();
                    setState(278);
                    symbol();
                    setState(279);
                    value();
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(281);
                    columnName();
                    setState(282);
                    symbol();
                    setState(283);
                    value();
                    setState(291);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                    while (_la == AND || _la == OR) {
                        {
                            {
                                setState(284);
                                binaryOperator();
                                setState(285);
                                columnName();
                                setState(286);
                                symbol();
                                setState(287);
                                value();
                            }
                        }
                        setState(293);
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

    public final BinaryOperatorContext binaryOperator() throws RecognitionException {
        BinaryOperatorContext _localctx = new BinaryOperatorContext(_ctx, getState());
        enterRule(_localctx, 54, RULE_binaryOperator);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(296);
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

    public final SymbolContext symbol() throws RecognitionException {
        SymbolContext _localctx = new SymbolContext(_ctx, getState());
        enterRule(_localctx, 56, RULE_symbol);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(298);
                _la = _input.LA(1);
                if (!((((_la) & ~0x3f) == 0 && ((1L << _la) & 135104L) != 0))) {
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

    public final ValueContext value() throws RecognitionException {
        ValueContext _localctx = new ValueContext(_ctx, getState());
        enterRule(_localctx, 58, RULE_value);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(300);
                _la = _input.LA(1);
                if (!((((_la) & ~0x3f) == 0 && ((1L << _la) & 3377699720531968L) != 0))) {
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

    public final ValuesInParenthesesContext valuesInParentheses() throws RecognitionException {
        ValuesInParenthesesContext _localctx = new ValuesInParenthesesContext(_ctx, getState());
        enterRule(_localctx, 60, RULE_valuesInParentheses);
        int _la;
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(302);
                match(OPEN_PAR);
                setState(303);
                value();
                setState(308);
                _errHandler.sync(this);
                _la = _input.LA(1);
                while (_la == COL) {
                    {
                        {
                            setState(304);
                            match(COL);
                            setState(305);
                            value();
                        }
                    }
                    setState(310);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                }
                setState(311);
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

    public final DatabaseURLContext databaseURL() throws RecognitionException {
        DatabaseURLContext _localctx = new DatabaseURLContext(_ctx, getState());
        enterRule(_localctx, 62, RULE_databaseURL);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(313);
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

    public final TableNameContext tableName() throws RecognitionException {
        TableNameContext _localctx = new TableNameContext(_ctx, getState());
        enterRule(_localctx, 64, RULE_tableName);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(315);
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

    public final TableDotColumnNameContext tableDotColumnName() throws RecognitionException {
        TableDotColumnNameContext _localctx = new TableDotColumnNameContext(_ctx, getState());
        enterRule(_localctx, 66, RULE_tableDotColumnName);
        try {
            enterOuterAlt(_localctx, 1);
            {
                setState(317);
                match(IDENTIFIER);
                setState(318);
                match(DOT);
                setState(319);
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

    public final ColumnNameContext columnName() throws RecognitionException {
        ColumnNameContext _localctx = new ColumnNameContext(_ctx, getState());
        enterRule(_localctx, 68, RULE_columnName);
        try {
            setState(325);
            _errHandler.sync(this);
            switch (getInterpreter().adaptivePredict(_input, 24, _ctx)) {
                case 1:
                    enterOuterAlt(_localctx, 1);
                {
                    setState(321);
                    match(IDENTIFIER);
                }
                break;
                case 2:
                    enterOuterAlt(_localctx, 2);
                {
                    setState(322);
                    match(IDENTIFIER);
                    setState(323);
                    match(DOT);
                    setState(324);
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

    @SuppressWarnings("CheckReturnValue")
    public static class RootContext extends ParserRuleContext {

        public RootContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public StatementContext statement() {
            return getRuleContext(StatementContext.class, 0);
        }

        public TerminalNode EOF() {
            return getToken(JFSQLParser.EOF, 0);
        }

        public TerminalNode SCOL() {
            return getToken(JFSQLParser.SCOL, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class StatementContext extends ParserRuleContext {

        public StatementContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public AlterTableContext alterTable() {
            return getRuleContext(AlterTableContext.class, 0);
        }

        public CreateTableContext createTable() {
            return getRuleContext(CreateTableContext.class, 0);
        }

        public CreateDatabaseContext createDatabase() {
            return getRuleContext(CreateDatabaseContext.class, 0);
        }

        public DeleteContext delete() {
            return getRuleContext(DeleteContext.class, 0);
        }

        public DropTableContext dropTable() {
            return getRuleContext(DropTableContext.class, 0);
        }

        public DropDatabaseContext dropDatabase() {
            return getRuleContext(DropDatabaseContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class AlterTableContext extends ParserRuleContext {

        public AlterTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class RenameTableContext extends ParserRuleContext {

        public RenameTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode RENAME() {
            return getToken(JFSQLParser.RENAME, 0);
        }

        public TerminalNode TO() {
            return getToken(JFSQLParser.TO, 0);
        }

        public TableNameContext tableName() {
            return getRuleContext(TableNameContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class RenameColumnContext extends ParserRuleContext {

        public RenameColumnContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class AddColumnContext extends ParserRuleContext {

        public AddColumnContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode ADD() {
            return getToken(JFSQLParser.ADD, 0);
        }

        public TerminalNode COLUMN() {
            return getToken(JFSQLParser.COLUMN, 0);
        }

        public ColumnDefinitionContext columnDefinition() {
            return getRuleContext(ColumnDefinitionContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class DropColumnContext extends ParserRuleContext {

        public DropColumnContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode DROP() {
            return getToken(JFSQLParser.DROP, 0);
        }

        public TerminalNode COLUMN() {
            return getToken(JFSQLParser.COLUMN, 0);
        }

        public ColumnNameContext columnName() {
            return getRuleContext(ColumnNameContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class CreateTableContext extends ParserRuleContext {

        public CreateTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class CreateDatabaseContext extends ParserRuleContext {

        public CreateDatabaseContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode CREATE() {
            return getToken(JFSQLParser.CREATE, 0);
        }

        public TerminalNode DATABASE() {
            return getToken(JFSQLParser.DATABASE, 0);
        }

        public DatabaseURLContext databaseURL() {
            return getRuleContext(DatabaseURLContext.class, 0);
        }

        @Override
        public int getRuleIndex() {
            return RULE_createDatabase;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterCreateDatabase(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitCreateDatabase(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitCreateDatabase(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    @SuppressWarnings("CheckReturnValue")
    public static class DeleteContext extends ParserRuleContext {

        public DeleteContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class DropTableContext extends ParserRuleContext {

        public DropTableContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class DropDatabaseContext extends ParserRuleContext {

        public DropDatabaseContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode DROP() {
            return getToken(JFSQLParser.DROP, 0);
        }

        public TerminalNode DATABASE() {
            return getToken(JFSQLParser.DATABASE, 0);
        }

        public DatabaseURLContext databaseURL() {
            return getRuleContext(DatabaseURLContext.class, 0);
        }

        @Override
        public int getRuleIndex() {
            return RULE_dropDatabase;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterDropDatabase(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitDropDatabase(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitDropDatabase(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    @SuppressWarnings("CheckReturnValue")
    public static class InsertContext extends ParserRuleContext {

        public InsertContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class SelectContext extends ParserRuleContext {

        public SelectContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

        public LimitContext limit() {
            return getRuleContext(LimitContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class LimitContext extends ParserRuleContext {

        public LimitContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode LIMIT() {
            return getToken(JFSQLParser.LIMIT, 0);
        }

        public NumericValueContext numericValue() {
            return getRuleContext(NumericValueContext.class, 0);
        }

        public OffsetContext offset() {
            return getRuleContext(OffsetContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class OffsetContext extends ParserRuleContext {

        public OffsetContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode OFFSET() {
            return getToken(JFSQLParser.OFFSET, 0);
        }

        public NumericValueContext numericValue() {
            return getRuleContext(NumericValueContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class NumericValueContext extends ParserRuleContext {

        public NumericValueContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode NUMERIC_LITERAL() {
            return getToken(JFSQLParser.NUMERIC_LITERAL, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class JoinOperationContext extends ParserRuleContext {

        public JoinOperationContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public InnerJoinContext innerJoin() {
            return getRuleContext(InnerJoinContext.class, 0);
        }

        public LeftJoinContext leftJoin() {
            return getRuleContext(LeftJoinContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class InnerJoinContext extends ParserRuleContext {

        public InnerJoinContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
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

        public TerminalNode INNER() {
            return getToken(JFSQLParser.INNER, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class LeftJoinContext extends ParserRuleContext {

        public LeftJoinContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class UpdateContext extends ParserRuleContext {

        public UpdateContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class ColumnDefinitionContext extends ParserRuleContext {

        public ColumnDefinitionContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public ColumnNameContext columnName() {
            return getRuleContext(ColumnNameContext.class, 0);
        }

        public ColumnTypeContext columnType() {
            return getRuleContext(ColumnTypeContext.class, 0);
        }

        public NotNullContext notNull() {
            return getRuleContext(NotNullContext.class, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class NotNullContext extends ParserRuleContext {

        public NotNullContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode NOT() {
            return getToken(JFSQLParser.NOT, 0);
        }

        public TerminalNode NULL() {
            return getToken(JFSQLParser.NULL, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class IfExistsContext extends ParserRuleContext {

        public IfExistsContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode IF() {
            return getToken(JFSQLParser.IF, 0);
        }

        public TerminalNode EXISTS() {
            return getToken(JFSQLParser.EXISTS, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class IfNotExistsContext extends ParserRuleContext {

        public IfNotExistsContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode IF() {
            return getToken(JFSQLParser.IF, 0);
        }

        public TerminalNode NOT() {
            return getToken(JFSQLParser.NOT, 0);
        }

        public TerminalNode EXISTS() {
            return getToken(JFSQLParser.EXISTS, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class ColumnTypeContext extends ParserRuleContext {

        public ColumnTypeContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class ExprContext extends ParserRuleContext {

        public ExprContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class BinaryOperatorContext extends ParserRuleContext {

        public BinaryOperatorContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode AND() {
            return getToken(JFSQLParser.AND, 0);
        }

        public TerminalNode OR() {
            return getToken(JFSQLParser.OR, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class SymbolContext extends ParserRuleContext {

        public SymbolContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class ValueContext extends ParserRuleContext {

        public ValueContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode NUMERIC_LITERAL() {
            return getToken(JFSQLParser.NUMERIC_LITERAL, 0);
        }

        public TerminalNode STRING_LITERAL() {
            return getToken(JFSQLParser.STRING_LITERAL, 0);
        }

        public TerminalNode QUESTION_MARK() {
            return getToken(JFSQLParser.QUESTION_MARK, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class ValuesInParenthesesContext extends ParserRuleContext {

        public ValuesInParenthesesContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

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

    @SuppressWarnings("CheckReturnValue")
    public static class DatabaseURLContext extends ParserRuleContext {

        public DatabaseURLContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode IDENTIFIER() {
            return getToken(JFSQLParser.IDENTIFIER, 0);
        }

        @Override
        public int getRuleIndex() {
            return RULE_databaseURL;
        }

        @Override
        public void enterRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).enterDatabaseURL(this);
					}
        }

        @Override
        public void exitRule(ParseTreeListener listener) {
					if (listener instanceof JFSQLListener) {
						((JFSQLListener) listener).exitDatabaseURL(this);
					}
        }

        @Override
        public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
					if (visitor instanceof JFSQLVisitor) {
						return ((JFSQLVisitor<? extends T>) visitor).visitDatabaseURL(this);
					} else {
						return visitor.visitChildren(this);
					}
        }
    }

    @SuppressWarnings("CheckReturnValue")
    public static class TableNameContext extends ParserRuleContext {

        public TableNameContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public TerminalNode IDENTIFIER() {
            return getToken(JFSQLParser.IDENTIFIER, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class TableDotColumnNameContext extends ParserRuleContext {

        public TableDotColumnNameContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public List<TerminalNode> IDENTIFIER() {
            return getTokens(JFSQLParser.IDENTIFIER);
        }

        public TerminalNode IDENTIFIER(int i) {
            return getToken(JFSQLParser.IDENTIFIER, i);
        }

        public TerminalNode DOT() {
            return getToken(JFSQLParser.DOT, 0);
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

    @SuppressWarnings("CheckReturnValue")
    public static class ColumnNameContext extends ParserRuleContext {

        public ColumnNameContext(ParserRuleContext parent, int invokingState) {
            super(parent, invokingState);
        }

        public List<TerminalNode> IDENTIFIER() {
            return getTokens(JFSQLParser.IDENTIFIER);
        }

        public TerminalNode IDENTIFIER(int i) {
            return getToken(JFSQLParser.IDENTIFIER, i);
        }

        public TerminalNode DOT() {
            return getToken(JFSQLParser.DOT, 0);
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
}