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

source install.sh



usage:

ne_run -N 2 -o rep_2.tex
ne_go_reports
ne_pub_reports


