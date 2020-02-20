#!/usr/local/bin/ipython
#-*- coding: utf-8 -*-


import os
import sys

import subprocess

import inspect
import datetime
import argparse

from IPython import embed


def is_none(obj):
	return isinstance(obj, type(None))



def f_print(*args, **kwargs):
	if len(args) >= 1:
		if type(args[0]) == str:
			if args[0][0] == "\n":
				print("\n")
				args = tuple([ args[0].lstrip() ] + list(args[1:]))
	prev_func_name = inspect.currentframe().f_back.f_code.co_name
	file_name = os.path.basename(__file__)
	current_time = datetime.datetime.strftime(datetime.datetime.now(), "%H:%M:%S")
	#print(f"[{ file_name }:{ prev_func_name }]", end = ": ")
	print(f"[{ current_time }:{ prev_func_name }]", end = ": ")
	print(*args, **kwargs)


class c_proc:


	cmd = None
	proc_info = None
	proc_name = None
	stdout = None
	stderr = None
	target_dir = None
	current_dir = None
	run_command = None
	full_command = None
	pid = None
	timeout = None


	def __init__(self):
		pass
		self.cmd = [ "uname", "-a" ]
		self.proc_info = None
		self.stdout = None
		self.stderr = None
		self.current_dir = os.getcwd()
		self.proc_name = None
		self.target_dir = None
		self.run_command = None
		self.full_command = None
		self.pid = None
		self.timeout = 30


	def set_proc_name(self, proc_name = None):
		if is_none(proc_name):
			if isinstance(self.cmd, list):
				if len(self.cmd) >= 1:
					self.proc_name = self.cmd[0]
		else:
			self.proc_name = proc_name


	def set_timeout(self, timeout):
		self.timeout = timeout


	def set(self, popen_cmd, prefix = False, timeout = -1):
		self.cmd = popen_cmd
		self.set_proc_name(proc_name = None)
		self.set_target_dir()
		self.set_run_command(prefix = prefix)
		pass


	def get(self):
		return self.cmd


	def set_target_dir(self):
		if not is_none(self.proc_name):
			self.target_dir = os.path.dirname(self.proc_name)
			if self.target_dir == "":
				self.target_dir = self.current_dir


	def set_run_command(self, prefix = False):
		if not is_none(self.proc_name):
			self.run_command = ( "./" if prefix else "" ) + f"{ os.path.basename(self.proc_name) }"
			self.full_command = self.run_command + " " +  " ".join(map(str, self.cmd[1:]))


	def go_target_dir(self):
		f_print(f"cd to: {self.target_dir}")
		os.chdir(self.target_dir)


	def go_back(self):
		f_print(f"cd to: {self.current_dir}")
		os.chdir(self.current_dir)


	def set_pid(self, pid):
		self.pid = pid


	def get_pid(self):
		return self.pid


	def poll(self):
		if not is_none(self.proc_info):
			return self.proc_info.poll()


	def is_end(self):
		ret_code = self.poll()
		if is_none(ret_code):
			return False
		else:
			return True


	def wait(self, timeout = None):
		if is_none(self.proc_info):
			return
		if is_none(timeout):
			timeout = self.timeout
		timeout_str = str(timeout)
		if timeout == -1:
			timeout = None
			timeout_str = "infinity"
		f_print(f"wait process with pid { self.pid }")
		f_print(f"timeout is { timeout_str }")
		ret_code = self.proc_info.wait(timeout = timeout)
		return ret_code


	def run(self):
		if self.cmd == [ ] or is_none(self.cmd):
			return 1
		#self.proc_info = subprocess.Popen(self.cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
		self.go_target_dir()
		self.proc_info = subprocess.Popen(self.full_command.split(" "), stdout = subprocess.PIPE, stderr = subprocess.PIPE)
		self.set_pid(self.proc_info.pid)
		self.go_back()
		return 0


	def read(self):
		if not is_none(self.proc_info):
			self.stdout = self.proc_info.stdout.read()
			self.stderr = self.proc_info.stderr.read()



	def show(self, encoding = "utf-8"):
		out = self.stdout.decode(encoding = encoding)
		err = self.stderr.decode(encoding = encoding)
		pid_ = self.get_pid()
		cmd_ = self.full_command
		f_print(f"pid: { pid_ }")
		f_print(f"cmd: { cmd_ }")
		f_print(f"\nstdout:\n{ out }")
		f_print(f"\nstderr:\n{ err }")


	def do(self):
		self.run()
		self.read()
		self.wait()
		if self.is_end():
			self.show()



def f_hello():
	#print(f"[{ __file__ }]: hello msg")
	f_print("run date and time is:")
	f_print(f"{ datetime.datetime.now() }")
	f_print("hello msg")
	pass


def f_parse_args(args = sys.argv):
	prog_name = os.path.basename(__file__)
	parser = argparse.ArgumentParser(prog = prog_name,
		formatter_class = argparse.ArgumentDefaultsHelpFormatter)
	parser.add_argument("-N", "--equation-order", type = int, default = 1, help = "Order of equation (N)")
	parser.add_argument("-o", "--output-report", type = str, default = "report.tex", help = "Name of output report file in .tex format")
	parsed_args = parser.parse_args(args)
	return parsed_args


def f_get_wls_info():
	default_wls_info = "/home/hjk/repos/nse_eq/wl_main/run_nse_eq.wls"
	wls_info = default_wls_info
	wls_info_env_var = "BIN_WLS_NSE_EQ"
	if wls_info_env_var  in os.environ.keys():
		wls_info = os.environ[wls_info_env_var]
		f_print(f"environment variable \"{ wls_info_env_var }\" is exists")
		f_print(f"use value of this environment variable as information about project wolfram language script file")
	else:
		f_print("use default information about project wolfram language script file")
	f_print(f"info: { wls_info }\n")
	return wls_info


def f_get_make_rep_info():
	default_info_ = "/home/hjk/repos/nse_eq/wl_main/make_report.sh"
	info_ = default_info_
	info_env_var = "BIN_MAKE_REP_NSE_EQ"
	if info_env_var  in os.environ.keys():
		info_ = os.environ[info_env_var]
		f_print(f"environment variable \"{ info_env_var }\" is exists")
		f_print(f"use value of this environment variable as information about project make report shell script")
	else:
		f_print("use default information about project make report shell script")
	f_print(f"info: { info_ }\n")
	return info_


def f_form_popen_cmd(wls_info = "", eq_order = "", output_rep = ""):
	popen_cmd = [ i if isinstance(i, str) else str(i) for i in [ wls_info, eq_order, output_rep ] ]
	return popen_cmd


def f_build_proc_2(rep_fn):
	info_ = f_get_make_rep_info()
	proc_mr = c_proc()
	proc_mr.set([ i if isinstance(i, str) else str(i) for i in [ info_, rep_fn ] ], prefix = True, timeout = 30)
	return proc_mr



def f_build_proc(popen_cmd):
	proc = c_proc()
	proc.set(popen_cmd, prefix = True, timeout = 30)
	return proc


def f_main():
	pass
	f_hello()
	f_print("\nafter hello function...")
	f_print("parse command line arguments...")
	parsed_args = f_parse_args(args = sys.argv[1:])
	eq_order = parsed_args.equation_order
	output_rep = parsed_args.output_report
	f_print(f"command line arguments is:")
	f_print(f"\nsys.argv[0]: { sys.argv[0] }")
	f_print(f"equation order (N): { eq_order }")
	f_print(f"output report file name: { output_rep }")
	f_print(f"\nget information about wolfram language script file:")
	wls_info = f_get_wls_info()
	wls_proc_cmd = f_form_popen_cmd(wls_info = wls_info, eq_order = eq_order, output_rep = output_rep)
	main_proc = f_build_proc(wls_proc_cmd)
	main_proc.set_timeout(timeout = -1)
	rep_proc = f_build_proc_2(output_rep)
	rep_proc.set_timeout(timeout = -1)
	return { "main" : main_proc, "misc" : rep_proc }


if __name__ == "__main__":
	f_print("initialize variable for future object of class \"c_proc\" with None value...")
	main_proc = None
	f_print("run main function...\n")
	proc_d = f_main()
	main_proc = proc_d["main"]
	rep_proc = proc_d["misc"]
	main_proc.do()
	rep_proc.do()
	pass


