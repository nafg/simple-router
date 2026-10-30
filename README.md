# simple-router
A flexible typesafe reversible router

## Scala.js

simple-router uses `java.time`, which Scala.js does not include, so a Scala.js application
that uses simple-router must add a `java.time` implementation itself, for example
[scala-java-time](https://github.com/cquiroz/scala-java-time):

```scala
libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "<version>"
```

Without one, linking fails with `Referring to non-existent class java.time.Instant`.
