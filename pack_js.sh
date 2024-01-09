#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

gleam build --target javascript

cd build/dev/javascript
rmdir -rf ./dist 2>/dev/null

NAME=kirala_markdown

npm install --save-exact --save-dev -g esbuild
#npm audit fix --force

#esbuild $NAME/$NAME.mjs --bundle --platform=node --outfile=$SCRIPT_DIR/build/$NAME.mjs
esbuild $NAME/$NAME.mjs --bundle --platform=node    --outfile=$SCRIPT_DIR/build/$NAME-node.js
esbuild $NAME/$NAME.mjs --bundle --platform=browser --outfile=$SCRIPT_DIR/build/$NAME-web.js
esbuild $NAME/$NAME.mjs --bundle --platform=neutral --outfile=$SCRIPT_DIR/build/$NAME.js