grammar JFSQL;

root
 : statement ( SCOL )? EOF
 ;

statement
 : alterTable
 | createTable
 | createDatabase
 | delete
 | dropTable
 | dropDatabase
 | insert
 | select
 | update
 ;

alterTable
 : ALTER TABLE tableName ( renameTable | renameColumn | addColumn | dropColumn )
 ;

renameTable
 : RENAME TO tableName
 ;

renameColumn
 : RENAME COLUMN columnName TO columnName
 ;

addColumn
 : ADD COLUMN columnDefinition
 ;

dropColumn
 : DROP COLUMN columnName
 ;

createTable
 : CREATE TABLE ( ifNotExists )? tableName OPEN_PAR columnDefinition ( COL columnDefinition )* CLOSE_PAR
 ;

createDatabase
 : CREATE DATABASE databaseURL
 ;

delete
 : DELETE FROM tableName ( WHERE expr )?
 ;

dropTable
 : DROP TABLE ( ifExists )? tableName
 ;

dropDatabase
 : DROP DATABASE databaseURL
 ;

insert
 : INSERT INTO tableName ( OPEN_PAR columnName ( COL columnName )* CLOSE_PAR )? VALUES valuesInParentheses ( COL valuesInParentheses )*
 ;

select
  : SELECT columnName ( COL columnName )* FROM tableName ( joinOperation )* ( WHERE expr )? ( limit )?
  ;

limit
 : LIMIT numericValue
 | LIMIT numericValue offset
 ;

offset
 : OFFSET numericValue
 ;

numericValue
 : NUMERIC_LITERAL
 ;

joinOperation
 : innerJoin
 | leftJoin
 ;

innerJoin
 : ( INNER )? JOIN tableName ON tableDotColumnName EQ tableDotColumnName
 ;

leftJoin
 : LEFT ( OUTER )? JOIN tableName ON tableDotColumnName EQ tableDotColumnName
 ;


update
 : UPDATE tableName SET columnName EQ value ( COL columnName EQ value )* ( WHERE expr )?
 ;

columnDefinition
 : columnName columnType ( notNull )?
 ;

notNull
 : NOT NULL;

ifExists
 : IF EXISTS
 ;

ifNotExists
 : IF NOT EXISTS
 ;

columnType
 : INTEGER
 | REAL
 | TEXT
 | BLOB
 ;

expr
 : columnName symbol value
 | columnName symbol value ( binaryOperator columnName symbol value )*
 ;

binaryOperator
 : AND
 | OR
 ;

symbol
 : EQ
 | NOT_EQ
 | LT
 | LT_EQ
 | GT
 | GT_EQ
 | LIKE
 ;

value
 : NUMERIC_LITERAL
 | STRING_LITERAL
 | QUESTION_MARK
 ;

valuesInParentheses
 : OPEN_PAR value ( COL value )* CLOSE_PAR
 ;

databaseURL
 : IDENTIFIER
 ;

tableName
 : IDENTIFIER
 ;

tableDotColumnName
 : IDENTIFIER DOT IDENTIFIER
 ;

columnName
 : IDENTIFIER
 | IDENTIFIER DOT IDENTIFIER
 ;

COL : ',';
SCOL : ';';
DOT : '.';
OPEN_PAR : '(';
CLOSE_PAR : ')';
LT : '<';
LT_EQ : '<=';
GT : '>';
GT_EQ : '>=';
EQ : '=';
NOT_EQ : '!=';
QUESTION_MARK : '?';

NOT : N O T;
NULL :  N U L L;
IF : I F;
EXISTS : E X I S T S;
LIKE : L I K E;
LIMIT : L I M I T;
OFFSET : O F F S E T;
AND : A N D;
OR: O R;
ADD : A D D;
ALTER : A L T E R;
CREATE : C R E A T E;
COLUMN : C O L U M N;
DELETE : D E L E T E;
DATABASE: D A T A B A S E;
DROP : D R O P;
FROM : F R O M;
INSERT : I N S E R T;
INTO : I N T O;
SELECT : S E L E C T;
SET : S E T;
TABLE : T A B L E;
UPDATE : U P D A T E;
VALUES : V A L U E S;
WHERE : W H E R E;
RENAME : R E N A M E;
TO : T O;
ON : O N;
LEFT : L E F T;
JOIN : J O I N;
INNER : I N N E R;
OUTER : O U T E R;
INTEGER : I N T E G E R;
REAL : R E A L;
TEXT : T E X T;
BLOB : B L O B;

IDENTIFIER
 : '"' ~'"'* '"'
 | '[' ~']'* ']'
 | [a-zA-Z_] [a-zA-Z_0-9]*
 | '*'
 ;

NUMERIC_LITERAL
 : DIGIT+ ( '.' DIGIT* )? ( E [-+]? DIGIT+ )?
 | '.' DIGIT+ ( E [-+]? DIGIT+ )?
 ;

STRING_LITERAL
 : '\'' ( ~'\'' | '\'\'' )* '\''
 ;

SPACES
 : [ \u000B\t\r\n] -> channel(HIDDEN)
 ;

fragment DIGIT : [0-9];
fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];