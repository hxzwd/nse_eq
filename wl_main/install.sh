#!/bin/bash

CURR_DIR=$(pwd)

export BIN_WLS_NSE_EQ="$CURR_DIR/run_nse_eq.wls"
#export BIN_MAKE_REP_NSE_EQ="$CURR_DIR/make_report.sh"
export BIN_MAKE_REP_NSE_EQ="$CURR_DIR/make_report.py"
export BIN_NSE_EQ="$CURR_DIR/run_nse_eq.py"
export NSE_EQ_PATH=$CURR_DIR


export NSE_EQ_SERVER_DIR="$HOME/www"
export NSE_EQ_PUB_REP="$NSE_EQ_SERVER_DIR/reports"


function ne_run()
{
	PREV_DIR=$(pwd)
	$NSE_EQ_PATH/./run_nse_eq.py -- $@
}

function ne_go_reports()
{
	cd $NSE_EQ_PATH/reports
}

function ne_gen_machine_info()
{
	CURR=$(pwd)
	cd $NSE_EQ_PATH
	#lshw -quiet -html > machine_info.html
	lshw -html -quiet 2> /dev/null > machine_info.html
	#> /dev/null 2> /dev/null
	weasyprint machine_info.html machine_info.pdf 2> /dev/null
	#-q > /dev/null 2> /dev/null
	TMP=$(pwd machine_info.pdf)
	cd $CURR
	echo $TMP
}

function ne_web()
{
	TMP=$(hostname -A)/reports.html
	TMP=$(echo $TMP | tr -d " ")
	echo $TMP

}

function ne_pub_reports()
{

	reports_file="$NSE_EQ_PATH/reports.html"

	echo "" > $reports_file
	echo "<title>REPORTS</title>" >> $reports_file
	echo "<body>" >> $reports_file
	echo "<hr>" >> $reports_file


	for rep in `ls $NSE_EQ_PATH/reports/*.pdf`
	do
		echo "copy report: $rep -> $NSE_EQ_PUB_REP/$(basename $rep)"
		cp $rep $NSE_EQ_PUB_REP/$(basename $rep)
		echo "<pre>" >> $reports_file
		echo -n "<a href = \"reports/$(basename $rep)\">$(basename $rep)</a>" >> $reports_file
		rep2=$(echo $rep | cut -d "." -f1)".tex"
		echo "copy report .tex: $rep2 -> $NSE_EQ_PUB_REP/$(basename $rep2)"
		cp $rep2 $NSE_EQ_PUB_REP/$(basename $rep2)
		echo -n -e "\t\t" >> $reports_file
		echo "<a href = \"reports/$(basename $rep2)\">TEX FILE: $(basename $rep2)</a>" >> $reports_file
		echo "</pre>" >> $reports_file
		#echo "<br>" >> $reports_file
	done

	echo "<br>" >> $reports_file
	echo "<hr>" >> $reports_file
	M_INFO_PATH=$(ne_gen_machine_info)
	M_INFO=$M_INFO_PATH/machine_info.pdf
	echo "copy machine info: $M_INFO > $NSE_EQ_PUB_REP/$(basename $M_INFO)"
	cp $M_INFO $NSE_EQ_PUB_REP/$(basename $M_INFO)
	echo "<a href = \"reports/$(basename $M_INFO)\">$(basename $M_INFO)</a>" >> $reports_file
	echo "<br>" >> $reports_file

	echo "<hr>" >> $reports_file
	echo "</body>" >> $reports_file
	echo "" >> $reports_file

	echo "copy .html reports file: $reports_file -> $NSE_EQ_SERVER_DIR/$(basename $reports_file)"
	cp $reports_file $NSE_EQ_SERVER_DIR/$(basename $reports_file)

}

