package com.github.jfsql.parser.dto;

import com.github.jfsql.parser.enums.TypeOfStatement;

public interface BaseStatement {

    TypeOfStatement getTypeOfStatement();
}
