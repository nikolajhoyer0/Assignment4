#!/bin/bash
rm src/*.beam
rm src.zip
zip -r src.zip src
curl -F src=@src.zip -F report=@report.pdf -F group=@group.txt https://ap16.onlineta.org/grade/4
