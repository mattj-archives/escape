#!/usr/bin/env bash

rm dist.zip
zip dist.zip escape.exe readme.txt stsg.exe gfx/* levels/* dosbox.conf dbrun.bat
rm /Users/mattj/projects/mjnet/content/games/escape/dist.zip
cp dist.zip /Users/mattj/projects/mjnet/content/games/escape/dist.zip

echo Done.
