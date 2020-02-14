#!/bin/bash

echo "MAKE REPORT"
echo

cd reports

echo -e "\n\n\n\n\n" | pdflatex report.tex

cd ../


