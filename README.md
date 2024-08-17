# BMLP
Thank you for visiting the GitHub repository of our paper _Boolean Matrix Logic Programming_.
Here, we explain:
- how to install BMLP modules
- how to use BMLP modules
- how to reproduce results in the paper

## Installation

BMLP can be installed as a module into existing SWI-Prolog code at TARGET_FOLDER by running the following commands:
```commandline
git clone git@github.com:lun-ai/BMLP.git
cp -r BMLP/ TARGET_FOLDER
```
BMLP depends on SWI-Prolog and we recommend version 9.2+.

## Using BMLP modules

We show an example from bmlp/tests.

```commandline
cd BMLP/
swipl -s example.pl -t rms_ex
```

```datalog
% Example 1 in paper
node(a).
node(b).
node(c).

edge(a,b).
edge(b,c).
```

BMLP methods and boolean operations are callable from bmlp.pl as a module in SWI-Prolog.
This module imports source code from the bmlp/ folder to support boolean matrix operations.

```datalog
:- use_module(bmlp).

 bmlp :- init('./temp'),
         compile('./bmlp/tests/ex_p0.pl',db(edge,[node,node],_),M1),
         rms(M1,M2,[output_id='path']),
         ln_print(M2).
```

**Initialisation:** BMLP modules need to be initialised to a folder to save intermediate computation results and the default is BMLP/temp/. 
If database has not been encoded as a boolean matrix, it can be compiled via the _compile_ method.
Otherwise, a matrix can be loaded using _lm_consult_ method.

**Compilation:** Target relation and object types in the database, e.g. edge(X:node,Y:node), are expressed to by the _db_ term.

**Boolean matrix computation:** This example calls BMLP-RMS module (Figure 2 in paper) and produce matrix M2 (basename "path").
M1 and M2 are matrices with the same format, represented by _matrix_ terms.
For example, M1 is grounded by ```matrix(edge, [node, node], [4, 4],_)``` 
since all entities are four nodes and its dimension is 4 x 4.
The transitive closure matrix M2 has been given an identifier "3".
```text
path3 (3x3):
         a b c
a       |0 1 1| % path(a, b). path(a, c). 
b       |0 0 1| % path(b, c).
c       |0 0 0|
```

Unit tests refer to examples in bmlp/tests which can be invoked by
```commandline
swipl -s bmlp.pl -t run_tests
```

## Reproducing results

Experiments need to be run from BMLP/. All experiments have 10 repetitions.
Non-BMLP methods runs can take up to many hours and some require installation (more details later).

### BMLP modules (BMLP-RMS & BMLP-SMP)

To run on datasets DG and DG+partial (Table 2 and Figure 4, 5): 
```commandline
cd BMLP/
bash run_exp.sh bmlp-rms full-5000 10
bash run_exp.sh bmlp-smp partial-5000 10
bash run_exp.sh bmlp-rms partial-range 10
cp experiments/path/full/results/* experiments/path/full/runtime/
cp experiments/path/partial/results/* experiments/path/partial/runtime/
```

To reproduce results on FB15K-237 [3] (Table 2):
```commandline
bash run_exp.sh bmlp-rms FB15K 10
cp experiments/FB15K/results/* experiments/FB15K/runtime/
```

### Non-BMLP systems

Skip to the next section to use the existing results for non-BMLP systems.
Otherwise, to get runtime of SYSTEM_NAME in DATASET:
```commandline
bash run_exp.sh SYSTEM_NAME DATASET 10
```
All results need to be copied to runtime/ folders for analysis.
```commandline
cp experiments/path/full/results/* experiments/path/full/runtime/
cp experiments/path/partial/results/* experiments/path/partial/runtime/
cp experiments/FB15K/results/* experiments/FB15K/runtime/
```
DATASET options are:
- partial-range
- partial-5000
- full-5000
- FB15K

SYSTEM_NAME options are:
- bpl:   B-Prolog [5] (binary in experiments/systems.zip)
- swipl: SWI-Prolog [4] ([install](https://www.swi-prolog.org/Download.html))
- clg: Clingo [1] ([install](https://github.com/potassco/clingo/releases/))
- souffle: Souffle [2] (binary in experiments/systems.zip)

#### Experimental data and analysis

One can analysis runtime results from the BMLP/. 
To generate statistical data in Table 2 and plot Figure 4 and 5:
```commandline
cd BMLP/
python experiments/runtime_analysis.py
```

Runtime results in the paper are stored at:
- experiments/path/full/runtime (CPU runtime of BMLP-RMS and other systems)
- experiments/path/partial/runtime (CPU runtime of BMLP-SMP and other systems)


## References

[1] M. Gebser, R. Kaminski, B. Kaufmann, and T. Schaub, ‘Clingo = ASP + Control: Preliminary Report’, Technical Communications of the Thirtieth International Conference on Logic Programming (ICLP’14), vol. 14, pp. 1–9, 2014.

[2] B. Scholz, H. Jordan, P. Subotić, and T. Westmann, ‘On fast large-scale program analysis in Datalog’, in Proceedings of the 25th International Conference on Compiler Construction, in CC 2016. New York, NY, USA: Association for Computing Machinery, Mar. 2016, pp. 196–206. doi: 10.1145/2892208.2892226.

[3] K. Toutanova and D. Chen, ‘Observed versus latent features for knowledge base and text inference’, in Proceedings of the 3rd Workshop on Continuous Vector Space Models and their Compositionality, A. Allauzen, E. Grefenstette, K. M. Hermann, H. Larochelle, and S. W. Yih, Eds., Beijing, China: Association for Computational Linguistics, 2015, pp. 57–66. doi: 10.18653/v1/W15-4007.

[4] J. Wielemaker, T. Schrijvers, M. Triska, and T. Lager, ‘SWI-Prolog’, Theory and Practice of Logic Programming, vol. 12, no. 1–2, pp. 67–96, 2012.

[5] N.-F. Zhou, ‘The language features and architecture of B-Prolog’, Theory and Practice of Logic Programming, vol. 12, no. 1–2, pp. 189–218, Jan. 2012, doi: 10.1017/S1471068411000445.
