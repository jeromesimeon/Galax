module namespace B = "server";

declare variable $dfullb :=
  <dir>
    <entry name="Bob" value="b"/>
    <entry name="Claire" value="c"/>
    <entry name="Denise" value="d"/>
  </dir>;

declare function B:stats() {
  doc("query_response_size_b")
};

declare function B:main() {
  $dfullb
};
