#!/bin/bash

trap '' HUP INT

############################## To call ##############################
# bash run_exp.sh SYSTEM_NAME DATASET REP


# Timeout 15000s but with some extra time for loading relations
OT=15100
cur_dir=$(pwd)

# check which datalog evaluation method to use
if [[ $1 == "all" ]]; then
  methods=("souffle")
else
  methods=(${1})
fi

# check which dataset to use
if [ $2 == "partial-5000" ]; then
    repo="experiments/path/partial"
    nodes=(5000)
    p=(0.01 0.1 0.5)
    echo "Dataset: ${2}, n:${nodes[@]}, pe:${p[@]}"
elif [ $2 == "full-5000" ]; then
    repo="experiments/path/full"
    nodes=(5000)
    p=(0.01 0.1 0.5)
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
            python scripts/extract_path.py --src_path ${repo}
            fn="${j}pe_${k}nodes.txt"
        else
            python scripts/extract_fb15k.py
            fn="FB15K.txt"
        fi
        for method in "${methods[@]}"; do
          case $method in
        # BMLP
            py-bmlp-gpu)
            rm -f ./test/*
            python ${repo}/py_bmlp.py
            ;;
        # Souffle
            souffle)
            cd ${repo}
            ${cur_dir}/experiments/bin/souffle -c --jobs=auto -F . -D . souffle.dl -p souffle.log
            ${cur_dir}/experiments/bin/souffleprof souffle.log -j=${j}pe_${k}nodes.html > /dev/null
            cat ${j}pe_${k}nodes.html | grep "data={" | sed 's/.*\[//' | sed 's/,.*//'
            rm -f *.html *.facts *.csv *.log *.cpp
            cd ${cur_dir} > /dev/null
            ;;
          esac>>${repo}/results/${method}_${fn}
        done
      done
  done
done
