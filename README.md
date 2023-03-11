# JFSQL

## Current state

- The driver currently supports the following SQL statements:
    * ALTER TABLE
    * CREATE DATABASE
    * CREATE TABLE
    * DELETE
    * DROP DATABASE
    * DROP TABLE
    * INSERT
    * SELECT (with INNER and LEFT JOIN)
    * UPDATE
- The driver supports two main output formats, json and XML.
    * For json we use json schemas which has json file extension, but the "Schema" postfix is added to the table's
      name (e.g. myTable.json and myTableSchema.json).
    * For XML we use XSD schemas for each table with the same name (e.g. myTable.xml and myTable.xsd).
- The driver uses git versioning framework (JGit) for transactions. The driver can be used in two modes:
    1. Committing mode - Every DML and DDL operation is committed (except DROP DATABASE). This is the safer option,
       because if the write operation fails for some reason, we can always roll back to the previous version and no data
       will be lost.
    2. Not committing mode - In this mode, the `commit()` will only take effect if `autoCommit` is set to `false`. JGit won't be used, and the `rollback()` method is not supported.
- BLOBs AND CLOBs are supported, but the Blob and Clob class from the java.sql package is not yet implemented
- Transactions are not guaranteed to work in multithreaded applications, but file locking mechanism was implemented.

## TODO:
- Improve overall test coverage# JFSQL
