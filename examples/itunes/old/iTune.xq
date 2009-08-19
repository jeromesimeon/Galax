declare variable $lib :=
(:  doc("/mnt/c/Documents\ and\ Settings/Administrator/My\ Documents/My\ Music/iTunes/iTunes\ Music\ Library.xml") :)
  doc("/home/simeon/it.xml")
;

declare function local:get_artists() {
  for $a in
    fn:distinct-values(for $x in $lib//key[.="Artist"]/following-sibling::*[1] return string($x))
  return
    <artist>{$a}</artist>
};

declare function local:get_keys() {
  for $a in
    fn:distinct-values(for $x in $lib//key[not(. castable as xs:integer)] return string($x))
  return
    <key>{$a}</key>
};

declare function local:get_tracks() {
  for $a in
    fn:distinct-values(for $x in $lib//key[(. castable as xs:integer)] return string($x))
  return
    <track>{$a}</track>
};

(:
local:get_artists();

local:get_tracks();
:)

<keys>{local:get_keys()}</keys>

