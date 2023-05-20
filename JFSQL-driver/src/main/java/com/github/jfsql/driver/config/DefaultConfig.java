package com.github.jfsql.driver.config;

import com.github.jfsql.driver.enums.Persistence;
import com.github.jfsql.driver.enums.TransactionVersioning;

public class DefaultConfig {

    static Persistence persistence = Persistence.JSON;
    static TransactionVersioning transactionVersioning = TransactionVersioning.DEFAULT;
    static boolean schemaValidation = true;
    static boolean statementCaching = true;

}
