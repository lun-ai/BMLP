#!/bin/bash

trap '' HUP INT

############################## To call ##############################
# bash run_exp.sh SYSTEM_NAME DATASET REP


# Timeout 15000s but with some extra time for loading relations
OT=15100
cur_dir=$(pwd)

# check which datalog evaluation method to use
if [[ $1 == "all" ]]; then
  methods=("bpl" "souffle" "clg" "swipl")
else
  methods=(${1})
fi

# check which dataset to use
if [ $2 == "partial-range" ]; then
    repo="experiments/path/partial"
    nodes=(1000 2000 3000 4000 5000)
    p=(0.001)
    echo "Dataset: ${2}, n:${nodes[@]}, pe:${p[@]}"
elif [ $2 == "partial-5000" ]; then
    repo="experiments/path/partial"
    nodes=(5000)
    p=(0.01 0.1 0.5)
    echo "Dataset: ${2}, n:${nodes[@]}, pe:${p[@]}"
elif [ $2 == "full-5000" ]; then
    repo="experiments/path/full"
    nodes=(5000)
    p=(0.0001 0.001 0.01 0.1 0.5 1)
    echo "Dataset: ${2}, n:${nodes[@]}, pe:${p[@]}"
elif [ $2 == "FB15K" ]; then
    repo="experiments/FB15K"
    nodes=(0)
    p=(0)
    echo "Dataset: ${2}"
else
    nodes=()
    echo "Cannot find dataset"
fi

# create results folder
mkdir -p ${repo}/results/

# iterate over experiment parameters
for k in "${nodes[@]}"; do
  for j in "${p[@]}"; do
      for i in $(seq 1 ${3}); do
        if [ $2 != "FB15K" ]; then
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
            tres=$(timeout ${OT}s swipl -s ${repo}/swi.pl --stack_limit=16000000000 -t 'compute' -q)
            if [ $? -eq 124 ]; then
            echo ${OT}
            else
            echo "${tres}"
            fi
            ;;
        # B-Prolog
            bpl)
            cd ${repo}
            tres=$(timeout ${OT}s ${cur_dir}/experiments/BProlog/bp -g "consult(b_prolog),compute")
            if [ $? -eq 124 ]; then
            echo ${OT}
            else
            echo "${tres}" | sed -n "4p"
            fi
            cd ${cur_dir} > /dev/null
            ;;
        # Clingo
            clg)
            python -m clingo -q --time-limit=${OT} ${repo}/background.lp ${repo}/clingo.lp | sed -n "9p" | sed 's/^.*: //' | sed 's/.$//'
            ;;
        # Souffle
            souffle)
            cd ${repo}
            ${cur_dir}/experiments/Souffle/build/src/souffle -c -F . -D . souffle.dl -p souffle.log
            ${cur_dir}/experiments/Souffle/build/src/souffleprof souffle.log -j=${j}pe_${k}nodes.html > /dev/null
            cat ${j}pe_${k}nodes.html | grep "data={" | sed 's/.*\[//' | sed 's/,.*//'
            rm -f *.html *.facts *.csv *.log *.cpp
            cd ${cur_dir} > /dev/null
            ;;
          esac>>${repo}/results/${method}_${fn}
        done
      done
  done
done
