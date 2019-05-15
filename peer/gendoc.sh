#!/bin/sh

tg=../web/website/static/api

shopt -s globstar

# opam switch 4.02.3+buckle-master
bsdoc build PocketMesh

cp -r docs/PocketMesh/PocketMeshPeer -T "$tg"
cp docs/odoc.css "$tg"
cp docs/highlight.pack.js "$tg"


sed -i "s:../../highlight.pack.js:highlight.pack.js:g" $tg/**/*.html
sed -i "s:../../odoc.css:odoc.css:g" $tg/**/*.html

# opam switch 4.06.1