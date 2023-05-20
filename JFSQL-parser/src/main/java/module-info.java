module JFSQL.parser {
    requires static lombok;
    requires org.antlr.antlr4.runtime;
    requires org.apache.logging.log4j;
    exports com.github.jfsql.parser.dto;
    exports com.github.jfsql.parser.core;
    exports com.github.jfsql.parser.generated;
    exports com.github.jfsql.parser.enums;
}