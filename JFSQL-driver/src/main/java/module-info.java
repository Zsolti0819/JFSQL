import com.github.jfsql.driver.core.JfsqlDriver;

module JFSQL.driver {
    requires java.sql;
    requires static lombok;
    requires JFSQL.parser;
    requires org.apache.logging.log4j;
    requires org.apache.commons.lang3;
    requires org.apache.commons.io;
    requires org.eclipse.jgit;
    requires com.google.gson;
    requires com.fasterxml.jackson.databind;
    requires json.schema.validator;
    provides java.sql.Driver with JfsqlDriver;
}
