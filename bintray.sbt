ThisBuild / publishTo :=
  Some("simple-router bintray" at "https://api.bintray.com/maven/naftoligug/maven/simple-router")

sys.env.get("BINTRAYKEY").toSeq map (key =>
  ThisBuild / credentials += Credentials("Bintray API Realm", "api.bintray.com", "naftoligug", key)
)
