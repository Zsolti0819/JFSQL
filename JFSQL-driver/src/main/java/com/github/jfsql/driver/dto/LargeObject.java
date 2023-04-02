package com.github.jfsql.driver.dto;

import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
public class LargeObject {

    private final String url;
    private final String value;

}
