package com.github.jfsql.driver.cache;

import com.github.jfsql.parser.dto.BaseStatement;
import java.util.Map;

public interface Cache {

    Map<String, BaseStatement> getCachedStatements();
}
