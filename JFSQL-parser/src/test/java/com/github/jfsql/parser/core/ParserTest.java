package com.github.jfsql.parser.core;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.github.jfsql.parser.dto.AlterTableStatement;
import com.github.jfsql.parser.dto.CreateDatabaseStatement;
import com.github.jfsql.parser.dto.CreateTableStatement;
import com.github.jfsql.parser.dto.DeleteStatement;
import com.github.jfsql.parser.dto.DropDatabaseStatement;
import com.github.jfsql.parser.dto.DropTableStatement;
import com.github.jfsql.parser.dto.InsertStatement;
import com.github.jfsql.parser.dto.SelectStatement;
import com.github.jfsql.parser.dto.UpdateStatement;
import java.util.Objects;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ParserTest {

    static Parser parser;

    @BeforeAll
    static void beforeAll() {
        parser = new Parser();
    }


    @ParameterizedTest
    @ValueSource(strings = {
        "",
        " ",
        "   ",
        "CREAT",
        "CREATE",
        "CREATEA",
        "CREATE ",
        "CREATE T",
        "CREATE TABLE",
        "CREATE TABLE a",
        "CREATE TABLE a ",
        "CREATE TABLE a(",
        "CREATE TABLE a)",
        "CREATE TABLE a (",
        "CREATE TABLE a )",
        "CREATE TABLE a ()",
        "CREATE TABLE a (a)",
        "CREATE TABLE a (a INTEGE)",
        "CREATE TABLE a (a INTEGE, b TEXT)",
        "CREATE TABLE   (a INTEGE, b TEXT)",
        "CREATE (a INTEGE, b TEXT)",
        "DELET",
        "DELETE ",
        "DELETE A",
        "DELETE  FROM ",
        "DROP",
        "DROPP ",
        "DROP ",
        "DROP TABLE",
        "DROP TABLE ",
        "INSER",
        "INSER",
        "INSERT INTO",
        "INSERT INTO a (A, B, ",
        "INSERT INTO a VALUES ()",
        "INSERT INTO a VALUES (",
        "INSERT INTO a VALUES )",
        "SELEC",
        "SELECT",
        "SELECT ",
        "SELECT table",
        "SELECT FROM",
        "SELECT FROM a",
        "SELECT a FROM B WHERE",
        "SELECT a FROM B WHERE id",
        "SELECT a FROM B WHERE id =",
        "UPDAT",
        "UPDATE",
        "UPDATE a",
        "UPDATE a SET b",
        "UPDATE a SET b =",
        "CREATE DATABASE C:\\Users\\f7ifsut\\IdeaProjects\\JFSQL-driver-main\\src\\main\\resources\\myDatabase",
        "UPDATE myTable SET name = 'Zsolti' WHERE id = 1 AND name = 'Not Zsolti' AND age = 25 AND name = 'Zsolti' AND id ;"
    })
    void testBadStatements(final String query) {
        assertNull(parser.parse(query));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "create table myTable (a integer, b real, c text, d blob)",
        "CREATE TABLE myTable (a INTEGER, b REAL, c TEXT, d BLOB);",
    })
    void testCreateTableStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), CreateTableStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "ALTER TABLE myTable RENAME COLUMN myColumn TO newMyColumn;",
        "alter table myTable rename column myColumn to newMyColumn;",
        "ALTER TABLE myTable RENAME TO myNewTable;",
        "ALTER TABLE myTable ADD COLUMN myNewColumn INTEGER;",
        "ALTER TABLE myTable DROP COLUMN columnToDrop;",
    })
    void testAlterTableStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), AlterTableStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "create database \"C:\\Users\\f7ifsut\\IdeaProjects\\JFSQL-driver-main\\src\\main\\resources\\myDatabase\"",
        "CREATE DATABASE \"C:\\Users\\f7ifsut\\IdeaProjects\\JFSQL-driver-main\\src\\main\\resources\\myDatabase\";",
        "CREATE DATABASE [C:\\Users\\f7ifsut\\IdeaProjects\\JFSQL-driver-main\\src\\main\\resources\\myDatabase];",
    })
    void testCreateDatabaseStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), CreateDatabaseStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "delete from myTable",
        "DELETE FROM myTable;",
        "DELETE FROM myTable WHERE name = 'Zsolti';",
        "DELETE FROM myTable WHERE name = 'Zsolti';",
        "DELETE FROM myTable WHERE name != 'Zsolti';",
        "DELETE FROM myTable WHERE id >= 1;",
        "DELETE FROM myTable WHERE id > 1;",
        "DELETE FROM myTable WHERE id <= 1;",
        "DELETE FROM myTable WHERE id = 1 OR name = 'Zsolti';",
        "DELETE FROM myTable WHERE id = 1 OR name = 'Zsolti' AND age = 25;",
    })
    void testDeleteStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), DeleteStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "drop database [C:\\Users\\destr\\IdeaProjects\\JFSQL-driver\\src\\main\\resources\\myDatabase]",
        "DROP DATABASE [C:\\Users\\destr\\IdeaProjects\\JFSQL-driver\\src\\main\\resources\\myDatabase];"
    })
    void testDropDatabaseStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), DropDatabaseStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "drop table myTable",
        "DROP TABLE myTable;",
    })
    void testDropTableStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), DropTableStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "INSERT INTO myTable (A, B, C) VALUES (1, 2, 3);",
        "insert into myTable (A, B, C) values (1, 2, 3);",
        "INSERT INTO myTable (A, B, C) VALUES ('1', '2', '3');",
        "INSERT INTO myTable VALUES (1, 2, 3);",
        "INSERT INTO myTable VALUES (1, 2, 3), (1, 2, 3), (1, 2, 3);",
        "INSERT INTO myTable VALUES (1, 2, 3), (1, 2, 3), (1, 2);",
    })
    void testInsertStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), InsertStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "select a from myTable",
        "SELECT * FROM myTable;",
        "SELECT * FROM myTable WHERE a = 'b';",
        "SELECT * FROM myTable WHERE a > 'b';",
        "SELECT * FROM myTable WHERE a >= 'b';",
        "SELECT * FROM myTable WHERE a < 'b';",
        "SELECT * FROM myTable WHERE a <= 'b';",
        "SELECT * FROM myTable WHERE a = 'b' AND c = 4;",
        "SELECT * FROM myTable WHERE id = 1 OR name = 'Zsolti' AND age = 25;",
    })
    void testSelectStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), SelectStatement.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "update a set b = 1",
        "UPDATE a SET b = 1;",
        "UPDATE a SET b = 1 WHERE c > 5;",
        "UPDATE a SET b = 1 WHERE c >= 5;",
        "UPDATE a SET b = 'b' WHERE c >= '5'",
        "UPDATE a SET b = 'b' WHERE id = 1 OR name = 'Zsolti' AND age = 25;",
    })
    void testUpdateStatements(final String query) {
        assertEquals(Objects.requireNonNull(parser.parse(query)).getClass(), UpdateStatement.class);
    }
}