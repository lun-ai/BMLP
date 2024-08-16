# BMLP
This repository hosts source code and experimental data for the paper _Boolean Matrix Logic Programming_.

We explain:
- how to install BMLP modules
- how to use BMLP modules
- how to reproduce results in the paper

## Installation

BMLP can be installed as a module into existing SWI-Prolog code at TARGET_FOLDER by running the following commands:
```
git clone git@github.com:lun-ai/BMLP.git
cp BMLP/bmlp.pl TARGET_FOLDER
cp -r BMLP/bmlp TARGET_FOLDER
```

## Using BMLP modules

We show an example from bmlp/tests.

```commandline
swipl -s example.pl -t ex_0
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
         compute(rms,M1,M2,[output_id='path']),
         ln_print(M2).
```

**Initialisation:** BMLP modules need to be initialised to a folder to save intermediate computation results and the default is directory root. 
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

### BMLP modules (BMLP-RMS & BMLP-SMP)

To run on datasets DG and DG+partial: 
```commandline
bash run_exp.sh BMLP-RMS full-5000 10     (Table 2 and Figure 4) 
bash run_exp.sh BMLP-SMP partial-5000 10  (Table 2 and Figure 4)
bash run_exp.sh BMLP-RMS partial-range 10 (Table 2 and Figure 5)
```

To reproduce results on FB15K-237 [3]:
```commandline
bash run_exp.sh BMLP-RMS FB15K 10   (Table 2)
```

### Non-BMLP systems (can take up to hours)

B-Prolog and Souffle binaries are included.
```commandline
unzip experiments/systems.zip
```

DATASET options are:
- partial-range
- partial-5000
- full-5000
- FB15K

SYSTEM_NAME other options (some require installation):
- bpl:   B-Prolog [5] (binary in zip)
- swipl: SWI-Prolog [4] (install from https://www.swi-prolog.org/Download.html)
- clingo: Clingo [1] (install from https://github.com/potassco/clingo/releases/)
- souffle: Souffle [2] (binary in zip or to build https://souffle-lang.github.io/build)

#### Experimental data and analysis

To generate statistical data in Table 2 and plot Figure 4 and 5:
```python
python experiments/runtime_analysis.py
```

CPU runtime of BMLP-RMS and other systems:
experiments/path/full/runtime

CPU runtime of BMLP-SMP and other systems:
experiments/path/partial/runtime

## References

[1] M. Gebser, R. Kaminski, B. Kaufmann, and T. Schaub, ‘Clingo = ASP + Control: Preliminary Report’, Technical Communications of the Thirtieth International Conference on Logic Programming (ICLP’14), vol. 14, pp. 1–9, 2014.

[2] B. Scholz, H. Jordan, P. Subotić, and T. Westmann, ‘On fast large-scale program analysis in Datalog’, in Proceedings of the 25th International Conference on Compiler Construction, in CC 2016. New York, NY, USA: Association for Computing Machinery, Mar. 2016, pp. 196–206. doi: 10.1145/2892208.2892226.

[3] K. Toutanova and D. Chen, ‘Observed versus latent features for knowledge base and text inference’, in Proceedings of the 3rd Workshop on Continuous Vector Space Models and their Compositionality, A. Allauzen, E. Grefenstette, K. M. Hermann, H. Larochelle, and S. W. Yih, Eds., Beijing, China: Association for Computational Linguistics, 2015, pp. 57–66. doi: 10.18653/v1/W15-4007.

[4] J. Wielemaker, T. Schrijvers, M. Triska, and T. Lager, ‘SWI-Prolog’, Theory and Practice of Logic Programming, vol. 12, no. 1–2, pp. 67–96, 2012.

[5] N.-F. Zhou, ‘The language features and architecture of B-Prolog’, Theory and Practice of Logic Programming, vol. 12, no. 1–2, pp. 189–218, Jan. 2012, doi: 10.1017/S1471068411000445.
