import matplotlib.pyplot as plt
import matplotlib.transforms as pltrans
import matplotlib.colors as mcolors
import collections

import numpy
from scipy.interpolate import make_interp_spline
import os
import json
import numpy as np
import re

colors = list(mcolors.TABLEAU_COLORS.values())

names = {
    'swipl': 'SWI-Prolog + tabling',
    'bmlp-rms': 'SWI-Prolog + BMLP-RMS',
    'bmlp-smp': 'SWI-Prolog + BMLP-SMP',
    'bpl': 'B-Prolog + tabling',
    'clg': 'Clingo',
    'xsbpl': 'XSB-Prolog + tabling',
    'souffle': 'Souffle'
}

# BMLP-IE and BMLP-RMS have two different colors
markers = {
    'swipl': ['x', colors[4]],
    'bmlp-rms': ['^', colors[1]],
    'bmlp-smp': ['^', colors[0]],
    'bpl': ['x', colors[3]],
    'clg': ['x', colors[2]],
    'xsbpl': ['x', colors[5]],
    'souffle': ['x', colors[6]]
}


# experiment 1
def compare_runtime(Path, pes, N, methods):
    data_list = []
    for f in os.listdir(Path):
        if '.txt' in f and str(N) in f and f.split('_')[0] in methods and len(f.split('_')) <= 3 and N == int(
                f.split('_')[2].strip('nodes.txt')):
            pe = float(f.split('_')[1].strip('pe'))
            if pe not in pes:
                continue
            data = {}
            seen = False
            data[-1] = f.split('_')[0]
            for d in data_list:
                if d[-1] == data[-1]:
                    data = d
                    seen = True
                    break
            file = open(Path + f)
            data[pe] = [float(l.strip()) for l in file.readlines() if isinstance(float(l.strip()), float)]
            if data_list == [] or not seen:
                data_list.append(data)
    sorted_data_list = []
    for d in data_list:
        sorted_data_list.append(collections.OrderedDict(sorted(d.items())))

    fig = plt.figure()
    for data in sorted_data_list:
        p = []
        u = []
        sterr = []
        print(data[-1])
        for p_ in data.keys():
            if p_ > 0:
                print('$' + str(np.around(np.mean(data[p_]), 3)) + ' \\pm ' + str(np.around(np.std(data[p_]), 3)) + '$')
                runtimes = [max(i, 0.001) for i in data[p_]]
                p.append(p_)
                u.append(min(np.mean(runtimes), 300))
                sterr.append(np.std(runtimes) / np.sqrt(len(runtimes)))
        plt.errorbar(np.log10(p), np.log10(u), markerfacecolor=markers[data[-1]][1], marker=markers[data[-1]][0],
                     color=markers[data[-1]][1],
                     markersize=7,
                     label=names[data[-1]], ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(np.log10(p), np.array(u) - np.array(sterr), np.array(u) + np.array(sterr), color=markers[data[-1]][2],
        #                  alpha=0.2)
        plt.xticks(np.round(np.log10(p), 2))
    plt.xlabel(r'Density of edges, $p_t$ ($log_{10}$)', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: list(names.keys())[list(names.values()).index(t[0])]))
    plt.legend(handles, labels, prop={'size': 13})
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_1_runtime.png')


def compare_runtime_2(Path, pe, MaxN, methods):
    data_list = []
    for f in os.listdir(Path):
        if '.txt' in f and str(pe) in f and f.split('_')[0] in methods:
            N = int(f.split('_')[2].strip('.txt').strip('nodes'))
            if N > MaxN:
                continue
            data = {}
            seen = False
            data[-1] = f.split('_')[0]
            for d in data_list:
                if d[-1] == data[-1]:
                    data = d
                    seen = True
                    break
            file = open(Path + f)
            data[int(N)] = [float(l.strip()) for l in file.readlines() if isinstance(float(l.strip()), float)]
            if data_list == [] or not seen:
                data_list.append(data)
    sorted_data_list = []
    for d in data_list:
        sorted_data_list.append(collections.OrderedDict(sorted(d.items())))

    fig = plt.figure()
    for data in sorted_data_list:
        p = []
        u = []
        sterr = []
        print(data[-1])
        for p_ in data.keys():
            if p_ > 0:
                print('$' + str(np.around(np.mean(data[p_]), 3)) + ' \\pm ' + str(np.around(np.std(data[p_]), 3)) + '$')
                runtimes = [max(i, 0.001) for i in data[p_]]
                p.append(p_)
                u.append(np.mean(runtimes))
                sterr.append(np.std(runtimes))
        plt.errorbar(p, np.log10(u), markerfacecolor=markers[data[-1]][1], marker=markers[data[-1]][0],
                     label=names[data[-1]],
                     color=markers[data[-1]][1],
                     markersize=7,
                     ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(p, np.array(u) - np.array(sterr), np.array(u) + np.array(sterr), color=markers[data[-1]][2],
        #                  alpha=0.2)
        plt.xticks(p)
    plt.xlabel('No. nodes', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: list(names.keys())[list(names.values()).index(t[0])]))
    plt.legend(handles, labels, prop={'size': 13})
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_1_runtime_2.png')


compare_runtime('experiments/connect/full/runtime/',
                [0.0001, 0.001, 0.01, 0.1, 0.5, 1],
                1000,
                ['bmlp-rms', 'clg', 'bpl', 'swipl', 'xsbpl', 'souffle'])
compare_runtime_2('experiments/connect/partial/runtime/',
                  0.001,
                  5000,
                  ['bmlp-smp', 'clg', 'bpl', 'swipl', 'xsbpl', 'souffle'])
