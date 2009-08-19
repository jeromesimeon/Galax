module namespace A = "server";

declare variable $A:dfull := fn:doc("path.xml");

declare function A:main() {
  $A:dfull/dir
};
