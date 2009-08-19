(: Q1: Delete all parts. :)
(: Q1: Delete all parts belonging to a car in "part-tree.xml", leaving the car itself. :)
delete nodes doc("docs/part-tree.xml")//part[@name="car"]/part
;
doc("docs/part-tree.xml")

