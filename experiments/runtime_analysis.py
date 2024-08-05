import matplotlib.pyplot as plt
import matplotlib.transforms as pltrans
import matplotlib.colors as mcolors
import collections
from scipy.interpolate import make_interp_spline
import os
import json
import numpy as np
import re

colors = list(mcolors.TABLEAU_COLORS.values())

# BMLP-IE and BMLP-RMS have two different colors
markers = {
    'swipl': ['SWI-Prolog + tabling', 'x', colors[4]],
    # 'bmlp-ie': ['SWI-Prolog + BMLP-IE', '^', colors[0]],
    'bmlp-rms': ['SWI-Prolog + BMLP-RMS', '^', colors[1]],
    'bmlp-smp': ['SWI-Prolog + BMLP-SMP', '^', colors[0]],
    'bpl': ['B-Prolog + tabling', 'x', colors[3]],
    'clg': ['Clingo', 'x', colors[2]],
    'xsbpl': ['XSB-Prolog + tabling', 'x', colors[5]],
    'souffle': ['Souffle', 'x', colors[6]]
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
        plt.errorbar(np.log10(p), np.log10(u), markerfacecolor=markers[data[-1]][2], marker=markers[data[-1]][1],
                     color=markers[data[-1]][2],
                     markersize=7,
                     label=markers[data[-1]][0], ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(np.log10(p), np.array(u) - np.array(sterr), np.array(u) + np.array(sterr), color=markers[data[-1]][2],
        #                  alpha=0.2)
        plt.xticks(np.round(np.log10(p), 2))
    plt.xlabel(r'Density of edges, $p_t$ ($log_{10}$)', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: t[0]))
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
        plt.errorbar(p, np.log10(u), markerfacecolor=markers[data[-1]][2], marker=markers[data[-1]][1],
                     label=markers[data[-1]][0],
                     color=markers[data[-1]][2],
                     markersize=7,
                     ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(p, np.array(u) - np.array(sterr), np.array(u) + np.array(sterr), color=markers[data[-1]][2],
        #                  alpha=0.2)
        plt.xticks(p)
    plt.xlabel('No. nodes', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: t[0]))
    plt.legend(handles, labels, prop={'size': 13})
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_1_runtime_2.png')


def compare_runtime_3(Path, pes, MaxN, Methods):
    data_list = []
    for f in os.listdir(Path):
        for pe in pes:
            if '.txt' in f and str(pe) in f:
                N = int(f.split('_')[2].strip('nodes.txt'))
                if N > MaxN:
                    continue
                data = {}
                seen = False
                data[-1] = f.split('_')[0]
                data[-2] = pe
                if data[-1] in Methods:
                    for d in data_list:
                        if d[-1] == data[-1] and d[-2] == data[-2]:
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
    lines = []
    for data in sorted_data_list:
        for data2 in sorted_data_list:
            if data[-2] in lines or len(lines) == len(pes):
                continue
            if data[-1] != Methods[0] or data2[-1] != Methods[1] or data[-2] != data2[-2]:
                continue
            p = []
            u = []
            sterr = []
            for p1 in data.keys():
                for p2 in data2.keys():
                    if 0 < p1 == p2:
                        u.append(np.mean(data[p1]) - np.mean(data2[p2]))
                        p.append(p1)
                        # u.append(np.mean(runtimes))
                        # sterr.append(np.std(runtimes) / np.sqrt(len(runtimes)))
                        sterr.append(0)
            plt.errorbar(np.log10(p), np.log10(u), yerr=sterr, markerfacecolor='none', marker=markers[data[-1]][1],
                         label=data[-2],
                         color=colors[len(lines) - 1],
                         ls='-', elinewidth=0.1, capsize=0)
            plt.xticks(p)
            lines.append(data[-2])
    plt.xlabel('No. nodes', fontsize=15)
    plt.ylabel('Mean CPU time differences (sec)', fontsize=15)
    plt.legend(prop={'size': 15})
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_1_runtime_3.png')


# experiment 2
def compare_GEM_runtime(path, Ns, methods):
    data_list = []
    for f in os.listdir(path):
        if '.txt' in f and f.split('_')[0] in methods:
            N = int(f.split('_')[1].strip('.txt'))
            if N not in Ns:
                continue
            data = {}
            seen = False
            data[-1] = f.split('_')[0]
            for d in data_list:
                if d[-1] == data[-1]:
                    data = d
                    seen = True
                    break
            file = open(path + f)
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
                runtimes = [i + 0.001 for i in data[p_]]
                print('$' + str(np.around(np.mean(data[p_]), 3)) + ' \\pm ' + str(np.around(np.std(data[p_]), 3)) + '$')
                p.append(p_)
                u.append(np.mean(runtimes))
                sterr.append(np.std(runtimes) / np.sqrt(len(runtimes)))
        plt.errorbar(np.log10(p), np.log10(u), markerfacecolor='none', marker=markers[data[-1]][1],
                     label=markers[data[-1]][0],
                     color=markers[data[-1]][2],
                     ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(np.log10(p), np.array(u) - np.array(sterr), np.array(u) + np.array(sterr), color=markers[data[-1]][2],
        #                  alpha=0.2)
        plt.xticks(np.log10(p))
        plt.yticks([-3, -2, -1, 0, 1, 2, 3])
    plt.xlabel('Number of substrates in the initial marking (log10)', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: t[0]))
    plt.legend(handles, labels, prop={'size': 13})
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_2_runtime.png')


compare_runtime('experiments/connect/full/runtime/',
                [0.0001, 0.001, 0.01, 0.1, 0.5, 1],
                1000,
                ['bmlp-rms', 'clg', 'bpl', 'swipl', 'xsbpl', 'souffle'])
compare_runtime_2('experiments/connect/partial/runtime/',
                  0.001,
                  5000,
                  ['bmlp-smp', 'clg', 'bpl', 'swipl', 'xsbpl', 'souffle'])
# compare_runtime_3('/home/lun/workspace/metabolic_network_engineering/src/experiments/networks/connection/runtime/',
#                   [0.005,0.01],
#                   5000,
#                   ['sqm', 'nsqm'])
# compare_GEM_runtime('/home/lun/workspace/metabolic_network_engineering/src/experiments/networks/iML1515/runtime/',
#                     [1,10,100,1000],
#                     ['nsqm', 'clg', 'bpl', 'swipl', 'xsbpl'])
