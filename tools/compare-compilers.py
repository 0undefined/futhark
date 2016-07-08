#!/usr/bin/env python
#
# Quick hack to compare performance changes when you're hacking on the
# compiler.

import os
import sys
import subprocess
import matplotlib

matplotlib.use('Agg') # For headless use

import numpy as np
import matplotlib.pyplot as plt

red = sys.argv[1] # first compiler
blue = sys.argv[2] # second compiler

compilers = [red, blue]
colours = ["#ff0000", "#0000bb"]

plotfile = 'comparison.pdf'

benchmarks = []
benchmark_files = {}

for arg in sys.argv[3:]:
    if os.path.isfile(arg):
        name, ext = os.path.splitext(arg)
        base = os.path.basename(name)
        benchmarks.append(base)
        benchmark_files[base] = arg
    else:
        for root, _, files in os.walk(arg):
            for f in files:
                name, ext = os.path.splitext(f)
                if ext == '.fut':
                    base = os.path.basename(name)
                    benchmarks.append(base)
                    benchmark_files[base] = os.path.join(root, f)

def parse_bench_output(s):
    def parse_line(line):
        words=line.split()
        dataset = words[1][:-1]
        runtime = float(words[2][:-2])
        return (dataset, runtime)
    return map(parse_line, s.splitlines())

results = {}

for b in benchmarks:
    results[b] = {}
    try:
        for c in compilers:
            print("Running benchmark %s with compiler %s" % (b,c))
            out = subprocess.check_output(
                ["futhark-bench", "--no-validate", "--compiler", c, benchmark_files[b]],
                stderr = sys.stdout
            )

            for (d,r) in parse_bench_output(out):
                if not d in results[b]:
                    results[b][d] = {}
                results[b][d][c] = r
    except subprocess.CalledProcessError:
        print('Failed.')
        results[b] = {}

width = 0.2

fig, ax = plt.subplots()
ax.set_ylabel('Speedup')
# Escape underscores in the labels because they will otherwise be
# interpreted by TeX.

xtics = []
allrects = []
offset = 0
for b in benchmarks:
    print('%s:' % b)
    for d in results[b].keys():
        print('  Dataset %s:' % d)
        runtimes = map(lambda c: results[b][d][c], compilers)
        speedups = map(lambda x: runtimes[0] / x, runtimes)

        for (c, s) in zip(compilers, speedups):
            print('    %s speedup:\t %.2f' % (c, s))

        K = len(speedups)

        rects = []
        xtics.append(offset + (K/2*width))
        allrects.append(ax.bar(offset + np.arange(K)*width,
                               speedups, width, color=colours))
        offset += width * (K+1)

ax.set_xticks(xtics)
ax.set_xticklabels(map(lambda b: b.replace('_', r'\_'), benchmarks), rotation='vertical')
plt.tick_params(axis='x', which='major', pad=20)

plt.grid(b=True, which='minor', color='#777777', linestyle='-')
ax.yaxis.grid(True, linestyle='-')

fig.set_size_inches(18.5, 6.5)
plt.rc('text', usetex=True)
plt.savefig(plotfile, bbox_inches='tight')
print('Speedup graph written to %s' % plotfile)
