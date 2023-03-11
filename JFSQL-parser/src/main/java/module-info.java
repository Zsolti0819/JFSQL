module JFSQL.parser {
    requires lombok;
    requires org.antlr.antlr4.runtime;
    requires org.apache.logging.log4j;
    exports com.github.jfsql.parser.dto;
    exports com.github.jfsql.parser.core;
    exports com.github.jfsql.parser.generated;
}