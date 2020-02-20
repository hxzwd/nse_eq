#!/bin/bash

CURR_DIR=$(pwd)

export BIN_WLS_NSE_EQ="$CURR_DIR/run_nse_eq.wls"
export BIN_MAKE_REP_NSE_EQ="$CURR_DIR/make_report.sh"
export BIN_NSE_EQ="$CURR_DIR/run_nse_eq.py"
export NSE_EQ_PATH=$CURR_DIR


function ne_run()
{
	PREV_DIR=$(pwd)
	$NSE_EQ_PATH/./run_nse_eq.py -- $@
}

function ne_go_reports()
{
	cd $NSE_EQ_PATH/reports
}




