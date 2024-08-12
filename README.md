# BMLP
This repository hosts source code and experimental data for the paper _Boolean Matrix Logic Programming_.
We provide instructions on usage and results re-generation.

## Installation

BMLP can be installed as a module into existing SWI-Prolog code at TARGET_FOLDER by running the following commands:
```
git clone git@github.com:lun-ai/BMLP.git
cp BMLP/bmlp.pl TARGET_FOLDER
cp -r BMLP/bmlp TARGET_FOLDER
```

In your code, import BMLP as a module.
```
:- use_module(bmlp).
```

## Using BMLP modules

Implementations of BMLP modules are callable from bmlp.pl, a module in SWI-Prolog.
This modules imports source code from the bmlp/ folder.
This folder contains methods to support boolean matrix operations in SWI-Prolog.

Some examples in examples/ show how to use BMLP modules.

```swipl -s examples/example.pl -t ex_1```

BMLP modules need to be initialised to a folder to save intermediate computation results and the default is directory root. 
If database has not been encoded as a boolean matrix, it can be compiled via the _compile_ method.

```
 bmlp :- init,
         compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
         compute(rms,[M1],[M2],[output_id='connect'],_).
```

Facts in the database, e.g. edge(X:node,Y:node), are referred to by the _db_ term to focuses on target relation 
and object types.
M1 and M2 are matrices with the same format, represented by _matrix_ terms.
For example, M1 is grounded by ```matrix(edge, [node, node], [10, 10],_)``` 
since all entities are nodes and its dimension is 10 x 10.

Examples have been provided in the examples/.
Existing unit tests refer to these examples which can be invoked by
```
swipl -s bmlp.pl -t "run_tests"
```

## Reproducing results

Run the following cmd in the BMLP/ root folder.

```bash run_exp.sh SYSTEM_NAME experiments/networks/connect/SUB_FOLDER REP```

SYSTEM_NAME maps to the following systems (some require installation):

```commandline
unzip experiments/systems.zip
```

- bpl:   B-Prolog (binary in BProlog/)
- swipl: SWI-Prolog (Install from https://www.swi-prolog.org/Download.html)
- clingo: Clingo (install from https://github.com/potassco/clingo/releases/)
- souffle: Souffle (binary in Souffle/ or following instructions from https://souffle-lang.github.io/build)

### Experimental data

CPU runtime of BMLP-RMS and other systems:
experiments/connect/full/runtime

CPU runtime of BMLP-SMP and other systems:
experiments/connect/partial/runtime