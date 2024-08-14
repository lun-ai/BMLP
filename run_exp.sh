#!/bin/bash

trap '' HUP INT

############################## To call ##############################
# bash run_exp.sh system_name experiments/networks/connect sub_folder

############################## connect ####################################
repo=${2}
cur_dir=$(pwd)

if [[ $1 == "all" ]]; then
  methods=("bpl" "souffle")
else
  methods=(${1})
fi

# check which dataset to use
if [ $2 == "experiments/connect/partial" ]; then
    nodes=(5000)
    p=(0.01 0.1 0.5)
#   p=(0.0001 0.001 0.01 0.1 0.5 1)
    echo "dataset: ${2}, n:${nodes}, pe:${p}"
elif [ $2 == "experiments/connect/full" ]; then
    nodes=(5000)
    p=(0.0001 0.001 0.01 0.1 0.5 1)
#     p=(0.001)
    echo "dataset: ${2}, n:${nodes}, pe:${p}"
else
    nodes=(0)
    p=(0)
    echo "dataset: ${2}"
fi

for k in "${nodes[@]}"; do
  for j in "${p[@]}"; do
      for i in $(seq 1 ${3}); do
        if [ $2 != "experiments/FB15K" ]; then
            swipl -s experiments/generate_BK.pl -g "generate_background($j,$k,'${repo}'),background_to_dl('${repo}'),halt" -q
            cp ${repo}/background.pl ${repo}/background.lp
            fn="${j}pe_${k}nodes.txt"
        else
            python ${repo}/extract_relation.py
            fn="FB15K.txt"
        fi
        for method in "${methods[@]}"; do
          case $method in
        # BMLP
            bmlp-smp)
            rm -f ./test/*
            swipl -s ${repo}/swi_bmlp.pl -t 'compute' -q
            ;;
            bmlp-rms)
            rm -f ./test/*
            swipl -s ${repo}/swi_bmlp.pl -t 'compute' -q
            ;;
        # SWI-Prolog
            swipl)
            swipl -s ${repo}/swi.pl --stack_limit=16000000000 -t 'compute' -q
            ;;
        # B-Prolog
            bpl)
            cd ${repo}
            ${cur_dir}/experiments/BProlog/bp -g "consult(b_prolog),compute" | sed -n "4p"
            cd ${cur_dir} > /dev/null
            ;;
        # Clingo
            clg)
            python -m clingo -q --time-limit=15000 ${repo}/background.lp ${repo}/clingo.lp | sed -n "9p" | sed 's/^.*: //' | sed 's/.$//'
            ;;
        # Souffle
            souffle)
            cd ${repo}
            ${cur_dir}/experiments/Souffle/src/souffle -c -F . -D . souffle.dl -p souffle.log
            ${cur_dir}/experiments/Souffle/src/souffleprof souffle.log -j=${j}pe_${k}nodes.html > /dev/null
            cat ${j}pe_${k}nodes.html | grep "data={" | sed 's/.*\[//' | sed 's/,.*//'
            rm -f *.html *.facts *.csv *.log *.cpp
            cd ${cur_dir} > /dev/null
            ;;
          esac>>./${method}_${fn}
        done
      done
  done
done
