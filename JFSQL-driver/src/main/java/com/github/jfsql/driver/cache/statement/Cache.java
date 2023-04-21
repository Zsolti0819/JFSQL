package com.github.jfsql.driver.cache.statement;

import com.github.jfsql.parser.dto.BaseStatement;
import java.util.Map;

public interface Cache {

    Map<String, BaseStatement> getCachedStatements();
}
