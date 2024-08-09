#!/bin/bash

trap '' HUP INT

############################## To call ##############################
# bash run_exp.sh system_name experiments/networks/connect sub_folder

############################## connect ####################################
repo=${2}
cur_dir=$(pwd)

if [[ $1 == "all" ]]; then
  methods=("xsbpl" "clg")
else
  methods=(${1})
fi

if [[ $3 == "partial" ]]; then
  nodes=(1000 2000 3000 4000 5000)
#  nodes=(1000 2000)
  p=(0.001)
else
  nodes=(5000)
#  p=(0.0001 0.001 0.01 0.1 0.5 1)
  p=(0.5)
fi

for k in "${nodes[@]}"; do
  for j in "${p[@]}"; do
      for i in $(seq 1 ${3}); do
        swipl -s experiments/generate_BK.pl -g "generate_background($j,$k,'${repo}'),halt" -q
        for method in "${methods[@]}"; do
          case $method in
    # BMLP
            bmlp-smp)
            swipl -s ${repo}/swi_bmlp.pl -t 'compute' -q
            ;;
            bmlp-rms)
            swipl -s ${repo}/swi_bmlp.pl -t 'compute' -q
            ;;
    # SWI-Prolog
            swipl)
            swipl -s ${repo}/swi.pl --stack_limit=16000000000 -t 'compute' -q
            ;;
    # B-Prolog
            bpl)
            cd ${repo}
            ${cur_dir}/experiments/BProlog/bp -s 40000000 -g "consult(b_prolog),compute" | sed -n "4p"
            cd ${cur_dir} > /dev/null
            ;;
    # XSB-Prolog
            xsbpl)
            cd ${repo}
            ${cur_dir}/experiments/XSB/bin/xsb -e "consult(xsb_prolog),compute." --quietload | sed 's/^.* | ?- //'
            cd ${cur_dir} > /dev/null
            ;;
    # Clingo
            clg)
            cp ${repo}/background.pl ${repo}/background.lp
            clingo -q --time-limit=15000 ${repo}/background.lp ${repo}/clingo.lp | sed -n "9p" | sed 's/^.*: //' | sed 's/.$//'
            ;;
    # Souffle
            souffle)
            swipl -s experiments/generate_BK.pl -g "background_to_dl('${repo}'),halt" -q
            cd ${repo}
            ${cur_dir}/experiments/Souffle/src/souffle -c -F . -D . souffle.dl -p souffle.log
            ${cur_dir}/experiments/Souffle/src/souffleprof souffle.log -j=${j}pe_${k}nodes.html > /dev/null
            cat ${j}pe_${k}nodes.html | grep "data={" | sed 's/.*\[//' | sed 's/,.*//'
            rm -f *.html *.facts *.csv *.log *.cpp
            cd ${cur_dir} > /dev/null
            ;;
          esac>>./${method}_${j}pe_${k}nodes.txt
        done
      done
  done
done
