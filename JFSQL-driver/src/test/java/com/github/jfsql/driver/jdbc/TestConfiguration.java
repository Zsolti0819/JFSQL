package com.github.jfsql.driver.jdbc;

import java.util.stream.Stream;
import lombok.experimental.UtilityClass;
import org.junit.jupiter.params.provider.Arguments;

@UtilityClass
public class TestConfiguration {

    static Stream<Arguments> configurations() {
        return Stream.of(
            Arguments.of("json", "jgit"),
            Arguments.of("json", "none"),
            Arguments.of("xml", "jgit"),
            Arguments.of("xml", "none")
        );
    }

}
