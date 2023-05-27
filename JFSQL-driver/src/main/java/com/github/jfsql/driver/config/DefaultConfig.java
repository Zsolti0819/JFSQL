package com.github.jfsql.driver.config;

import com.github.jfsql.driver.enums.Persistence;
import com.github.jfsql.driver.enums.TransactionVersioning;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DefaultConfig {

    Persistence persistence = Persistence.JSON;
    TransactionVersioning transactionVersioning = TransactionVersioning.DEFAULT;
    boolean schemaValidation = true;
    boolean statementCaching = true;

}
