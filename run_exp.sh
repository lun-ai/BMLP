#!/bin/bash

trap '' HUP INT

############################## To call ##############################
# bash run_exp.sh system_name experiments/networks/connect sub_folder

############################## connect ####################################
method=$1
repo=${2}/${3}

for k in 1000 2000 3000 4000 5000
do
  for j in 0.001
  do
#for k in 1000
#do
#  for j in 0.0001 0.001 0.01 0.1 0.5 1
#  do
      for i in $(seq 1 10)
      do
        swipl -s ${repo}/generate_BK.pl -g "generate_background($j,$k),halt" -q
        case $method in
  # BMLP
          bmlp-smp)
          swipl -s ${repo}/swi.pl -t 'test(rmss)' -q
          ;;
#          bmlp-ime)
#          swipl -s ${repo}/swi.pl -t 'test(ime)' -q
#          ;;
          bmlp-rms)
          swipl -s ${repo}/swi.pl -t 'test(rms)' -q
          ;;
  # SWI-Prolog
          swipl)
          swipl -s ${repo}/swi.pl -t 'test(swipl)' -q
          ;;
  # B-Prolog
          bpl)
          cd ${repo}
          ~/workspace/BProlog/bp -s 40000000 -g "consult(b_prolog),compute" | sed -n "4p"
          cd ../../../../
          ;;
  # XSB-Prolog
          xsbpl)
          cd ${repo}
          ~/workspace/XSB/bin/xsb -e "consult(xsb_prolog),compute." --quietload | sed 's/^.* | ?- //'
          cd ../../../../
          ;;
  # Clingo
          clg)
          cp ${repo}/background.pl ${repo}/background.lp
          clingo -q --time-limit=5000 ${repo}/background.lp ${repo}/clingo.lp | sed -n "9p" | sed 's/^.*: //' | sed 's/.$//'
          ;;
  # Souffle
          souffle)
          swipl -s ${repo}/generate_BK.pl -g "conversion_background_to_dl,halt" -q
          cd ${repo}
          souffle -c -F . -D . souffle.dl -p souffle.log
          souffleprof souffle.log -j=${j}pe_${k}nodes.html > /dev/null
          cat ${j}pe_${k}nodes.html | grep "data={" | sed 's/.*\[//' | sed 's/,.*//'
          rm -f *.html *.facts *.csv *.log *.cpp
          cd ../../../../
          ;;
        esac
      done>./${method}_${j}pe_${k}nodes.txt
  done
done