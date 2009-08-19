#!/bin/sh

rm -f anim.gif
for i in *.png; do convert $i ${i/.png/.gif}; done
gifsicle -l -d 50 *.gif > anim.gif
