package com.github.jfsql.driver.util;

import java.util.Map;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DatatypeConverter {

    private static final Map<String, String> SQL_TO_XS_MAP = Map.of(
        "INTEGER", "xs:long",
        "REAL", "xs:float",
        "TEXT", "xs:string",
        "BLOB", "xs:anyURI"
    );
    private static final Map<String, String> XS_TO_SQL_MAP = Map.of(
        "xs:long", "INTEGER",
        "xs:float", "REAL",
        "xs:string", "TEXT",
        "xs:anyURI", "BLOB"
    );
    private static final Map<String, String> JSON_TO_SQL_MAP = Map.of(
        "integer", "INTEGER",
        "number", "REAL",
        "string", "TEXT"
    );
    private static final Map<String, String> SQL_TO_JSON_MAP = Map.of(
        "INTEGER", "integer",
        "REAL", "number",
        "TEXT", "string",
        "BLOB", "string"
    );
    private static final Map<String, String> SQL_TO_JAVA_MAP = Map.of(
        "INTEGER", "Long",
        "REAL", "Double",
        "TEXT", "String",
        "BLOB", "String"
    );

    public String convertFromSqlToXs(final String sqlDatatype) {
        final String xsEquivalent = SQL_TO_XS_MAP.get(sqlDatatype);
        if (xsEquivalent == null) {
            throw new IllegalStateException("Unknown SQL datatype \"" + sqlDatatype + "\"");
        }
        return xsEquivalent;
    }

    public String convertFromXsToSql(final String xsDatatype) {
        final String sqlEquivalent = XS_TO_SQL_MAP.get(xsDatatype);
        if (sqlEquivalent == null) {
            throw new IllegalStateException("Unknown xs datatype \"" + xsDatatype + "\"");
        }
        return sqlEquivalent;
    }

    public String convertFromJsonToSql(final String jsonDatatype) {
        final String sqlEquivalent = JSON_TO_SQL_MAP.get(jsonDatatype);
        if (sqlEquivalent == null) {
            throw new IllegalStateException("Unknown json datatype \"" + jsonDatatype + "\"");
        }
        return sqlEquivalent;
    }

    public String convertFromSqlToJson(final String sqlDatatype) {
        final String jsonEquivalent = SQL_TO_JSON_MAP.get(sqlDatatype);
        if (jsonEquivalent == null) {
            throw new IllegalStateException("Unknown SQL datatype \"" + sqlDatatype + "\"");
        }
        return jsonEquivalent;
    }

    String convertFromSqlToJava(final String sqlDatatype) {
        final String javaEquivalent = SQL_TO_JAVA_MAP.get(sqlDatatype);
        if (javaEquivalent == null) {
            throw new IllegalStateException("Unknown SQL datatype \"" + sqlDatatype + "\"");
        }
        return javaEquivalent;
    }
}
