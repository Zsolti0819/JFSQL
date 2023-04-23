package com.github.jfsql.driver.config;

public class DefaultConfig {

    static Persistence persistence = Persistence.JSON;
    static TransactionVersioning transactionVersioning = TransactionVersioning.JGIT;
    static boolean schemaValidation = true;
    static boolean statementCaching = true;

}
