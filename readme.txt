nse_eq
mephi, M, sem 3, research work


install:

mkdir -p /home/hjk/repos
cd /home/hjk/repos

git clone https://github.com/hxzwd/nse_eq.git

cd nse_eq
cd wl_main

#configure installation by editin install.sh script
#
#
#

source install.sh



usage:

ne_run -N 2 -o rep_2.tex
ne_go_reports
ne_pub_reports


machine:

cpu: Intel(R) Xeon(R) CPU E5645  @ 2.40GHz
cpu MHz: 2395.000
cpu width: 64 bits
cpu vendor: Intel Corp.
cpu cores: 6
cpu cores available: 1

memory total: 1048576 kB
