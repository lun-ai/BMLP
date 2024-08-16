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
    'swipl': 'SWI-Prolog*',
    'bmlp-rms': 'BMLP-RMS',
    'bmlp-smp': 'BMLP-SMP',
    'bpl': 'B-Prolog*',
    'clg': 'Clingo',
    'xsbpl': 'XSB-Prolog*',
    'souffle': 'Souffle',
    'ot': 'OT'
}

# BMLP-IE and BMLP-RMS have two different colors
markers = {
    'swipl': ['x', colors[4]],
    'bmlp-rms': ['^', colors[1]],
    'bmlp-smp': ['^', colors[0]],
    'bpl': ['x', colors[3]],
    'clg': ['x', colors[2]],
    'xsbpl': ['x', colors[5]],
    'souffle': ['x', colors[6]],
    'ot': ['--', 'black']
}

OT = 15000


# Figure 4
def plot_DG(Path, pes, N, methods):
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
                print('$' + str(np.around(np.mean(data[p_]), 2)) + ' \\pm ' + str(np.around(np.std(data[p_]), 2)) + '$')
                runtimes = [max(i, 0.001) for i in data[p_]]
                p.append(p_)
                u.append(min(np.mean(runtimes), OT))
                sterr.append(np.std(runtimes) / np.sqrt(len(runtimes)))
        plt.errorbar(np.log10(p), np.log10(u), markerfacecolor=markers[data[-1]][1], marker=markers[data[-1]][0],
                     color=markers[data[-1]][1],
                     markersize=7,
                     label=names[data[-1]], ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(np.log10(p), np.array(u) - np.array(sterr), np.array(u) + np.array(sterr),
        #                  color=markers[data[-1]][1],
        #                  alpha=0.2)
        plt.xticks(np.round(np.log10(p), 2))
    plt.xlabel(r'Density of edges $p_t$ ($log_{10}$)', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(
        *sorted(zip(labels, handles), key=lambda t: list(names.keys())[list(names.values()).index(t[0])]))
    plt.legend(handles, labels, prop={'size': 13}, loc='lower right')
    plt.axhline(y=np.log10(15000), color='black', ls='--',label="OT")
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_1_runtime.png')


# Figure 5
def plot_DG_partial(Path, pe, MaxN, methods):
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
                print('$' + str(np.around(np.mean(data[p_]), 2)) + ' \\pm ' + str(np.around(np.std(data[p_]), 2)) + '$')
                runtimes = [max(i, 0.001) for i in data[p_]]
                p.append(p_)
                u.append(np.mean(runtimes))
                sterr.append(np.std(runtimes))
        plt.errorbar(p, np.log10(u), markerfacecolor=markers[data[-1]][1], marker=markers[data[-1]][0],
                     label=names[data[-1]],
                     color=markers[data[-1]][1],
                     markersize=7,
                     ls='-', elinewidth=0.1, capsize=0)
        # plt.fill_between(p, np.array(u) - np.array(sterr), np.array(u) + np.array(sterr), color=markers[data[-1]][1],
        #                  alpha=0.2)
        plt.xticks(p)
    plt.xlabel(r'No. constants $n$', fontsize=15)
    plt.ylabel(r'Mean CPU time (seconds in $log_{10}$)', fontsize=15)
    # plt.ylabel('Mean CPU time (seconds)', fontsize=15)
    handles, labels = plt.gca().get_legend_handles_labels()
    labels, handles = zip(
        *sorted(zip(labels, handles), key=lambda t: list(names.keys())[list(names.values()).index(t[0])]))
    plt.legend(handles, labels, prop={'size': 13})
    c = 0.000006**2
    plt.errorbar([1000, 2000, 3000, 4000, 5000],
                 np.log10([1000 ** 3 * c, 2000 ** 3 * c, 3000 ** 3 * c, 4000 ** 3 * c, 5000 ** 3 * c]),
                 label=r'log_{10}(n^3) + c',
                 color='grey',
                 ls='--')
    plt.tight_layout(pad=0.4)
    plt.savefig('figures/exp_1_runtime_2.png')


def analysis_DG(Path, pes, N, methods):
    print(Path)
    data_list = []
    for f in os.listdir(Path):
        if '.txt' in f and (str(N) in f and f.split('_')[0] in methods and len(f.split('_')) <= 3 and N == int(
                f.split('_')[2].strip('nodes.txt'))):
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

    for data in sorted_data_list:
        p = []
        u = []
        sterr = []
        print(data[-1])
        for p_ in data.keys():
            if p_ > 0:
                print('$' + str(np.around(np.mean(data[p_]), 2)) + ' \\pm ' + str(np.around(np.std(data[p_]), 2)) + '$')
                runtimes = [max(i, 0.001) for i in data[p_]]
                p.append(p_)
                u.append(min(np.mean(runtimes), OT))
                sterr.append(np.std(runtimes) / np.sqrt(len(runtimes)))


def analysis_FB15K(Path):
    print(Path)
    data_list = []
    for f in os.listdir(Path):
        if '.txt' in f:
            data = {}
            seen = False
            data[-1] = f.split('_')[0]
            for d in data_list:
                if d[-1] == data[-1]:
                    data = d
                    seen = True
                    break
            file = open(Path + f)
            data['data'] = [float(l.strip()) for l in file.readlines() if isinstance(float(l.strip()), float)]
            if data_list == [] or not seen:
                data_list.append(data)
    sorted_data_list = data_list

    for data in sorted_data_list:
        p = []
        u = []
        sterr = []
        print(data[-1])
        for p_ in data.keys():
            if p_ != -1:
                print('$' + str(np.around(np.mean(data[p_]), 2)) + ' \\pm ' + str(np.around(np.std(data[p_]), 2)) + '$')
                runtimes = [max(i, 0.001) for i in data[p_]]
                p.append(p_)
                u.append(min(np.mean(runtimes), OT))
                sterr.append(np.std(runtimes) / np.sqrt(len(runtimes)))

# mean and sterr data in Table 2
# DG
analysis_DG('experiments/connect/full/runtime/',
                [0.01, 0.1, 0.5],
                5000,
                ['bmlp-rms', 'clg', 'bpl', 'swipl', 'souffle'])
# DG+partial
analysis_DG('experiments/connect/partial/runtime/',
                [0.01, 0.1, 0.5],
                5000,
                ['bmlp-smp', 'clg', 'bpl', 'swipl', 'souffle'])
# FB15K-237
analysis_FB15K('experiments/FB15K/runtime/')


# plot Figure 4
plot_DG('experiments/connect/full/runtime/',
        [0.0001, 0.001, 0.01, 0.1, 0.5, 1],
        5000,
        ['bmlp-rms', 'clg', 'bpl', 'swipl', 'souffle'])
# plot Figure 5
plot_DG_partial('experiments/connect/partial/runtime/',
                0.001,
                5000,
                ['bmlp-smp', 'clg', 'bpl', 'swipl', 'souffle'])
