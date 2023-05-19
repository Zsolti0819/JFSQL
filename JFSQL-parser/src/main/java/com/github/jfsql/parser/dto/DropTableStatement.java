package com.github.jfsql.parser.dto;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
public class DropTableStatement implements DropTableWrapper {

    private final String tableName;
    private final boolean ifExistsPresent;

}
