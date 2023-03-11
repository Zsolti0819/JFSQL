package com.github.jfsql.parser.dto;

import java.util.List;

public interface StatementWithWhere {

    List<String> getWhereColumns();

    List<String> getWhereValues();

    List<String> getSymbols();

    List<String> getBinaryOperators();
}
