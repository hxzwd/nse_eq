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
		echo "<a href = \"reports/$(basename $rep)\">$(basename $rep)</a>" >> $reports_file
		echo "<br>" >> $reports_file
	done

	echo "<hr>" >> $reports_file
	echo "</body>" >> $reports_file
	echo "" >> $reports_file

	echo "copy .html reports file: $reports_file -> $NSE_EQ_SERVER_DIR/$(basename $reports_file)"
	cp $reports_file $NSE_EQ_SERVER_DIR/$(basename $reports_file)

}

