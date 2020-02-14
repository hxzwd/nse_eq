#!/bin/bash

TEX_FILE=$1


echo "MAKE REPORT"
echo "TEX_FILE = $TEX_FILE"


cd reports

#echo -e "\n\n\n\n\n" | pdflatex report.tex
echo -e "\n\n\n\n\n" | pdflatex $TEX_FILE

cd ../


