# BMLP
This is the GitHub repository for the paper _Boolean Matrix Logic Programming on the GPU_.

## Installation

BMLP can be installed as a module into existing SWI-Prolog code at TARGET_FOLDER by running the following commands:
```commandline
git clone git@github.com:lun-ai/BMLP.git
cp -r BMLP/ TARGET_FOLDER
```
BMLP depends on SWI-Prolog and we recommend version 9.2+.
Additional Python packages are required if using BMLP with GPU and PyTorch.

## Usage

We showcase usage of BMLP in a simple directed graph with 3 nodes and 2 edges. This has the following incidence matrix.

```datalog
node(a).
node(b).
node(c).

         a b c
a       |0 1 0| % edge(a, b). 
b       |0 0 1| % edge(b, c).
c       |0 0 0|
```

### BMLP on GPU

BMLP on GPU aims to be Pythonic through PyTorch. This is implemented in the BMLP_GPU submodule.

```python
import torch
from bmlp.core.tensor import *
from bmlp.core.utils import *

# Extract relations from a Prolog file and create matrices
unary, binary = extract_relations_from_file('bmlp/tests/ex_p0.pl')
data = create_matrices_from_relations('edge',
                                      ['node', 'node'],
                                      unary, binary)

# Convert the data to a PyTorch tensor and apply RMS
m1 = torch.tensor(data['matrix'],
                  dtype=D_TYPE)
m2 = RMS(m1)

# Print the result
print('RMS result:\n', m2)
print_relations(convert_matrix_to_relations(
    m2, data['index_to_entity']['node']))

```
BMLP_GPU provides additional wrappers for Boolean matrix operations. Extending existing modules or operators are easy through PyTorch. 

### BMLP with SWI-Prolog

BMLP methods and boolean operations are callable from bmlp.pl as a module in SWI-Prolog.
This module imports source code from the bmlp/ folder to support boolean matrix operations.

```Prolog
:- use_module(bmlp).

bmlp_ex :- init('./temp'),
           compile('bmlp/tests/ex_p0.pl',db(edge,[node,node],_),M1),
           rms(M1,M2,[output_name='path']),
           lm_print(M2).
```
Calling the goal _bmlp_ex_ prints the output from the BMLP-RMS module. 
```commandline
swipl -s example.pl -t rms_ex
```

```text
path3 (3x3):
         a b c
a       |0 1 1| % path(a, b). path(a, c). 
b       |0 0 1| % path(b, c).
c       |0 0 0|
```
One can convert a matrix into facts by adding the following body to bmlp_ex. 
This would print out the list [path(a,b), path(a,c), path(b,c)].
```Prolog
...
lm_to_facts(M2,Fs),
writeln(Fs).
```

<details>
  <summary>
  More details
  </summary>
  <p>

  **Initialisation:** BMLP modules need to be initialised to a folder to save intermediate computation results and the default is BMLP/temp/. 
If a database has not been encoded as a boolean matrix, it can be compiled via the _compile_ method.
Otherwise, a matrix can be loaded using _lm_consult_ method.

**Compilation:** Target relation and object types in the database, e.g. edge(X:node,Y:node), are expressed by the _db_ term.

**Boolean matrix computation:** This example calls BMLP-RMS module (Figure 2 in paper) and produce matrix M2 (basename "path").
M1 and M2 are matrices with the same format, represented by _matrix_ terms.
For example, M1 is grounded by ```matrix([edge,1], [node, node], [3, 3],_)``` 
since all entities are 3 nodes and its dimension is 3 x 3.
The transitive closure matrix M2 has been given an identifier "3".

**BMLP test suite:** Unit tests of compilation, BMLP modules and boolean matrix operations refer to examples in 
bmlp/tests which can be invoked by
```commandline
swipl -s bmlp.pl -t run_tests
```
</p>
</details>



## Reproducing results

Experiments need to be run from BMLP/. All experiments have 10 repetitions.
Non-BMLP methods runs can take up to many hours and some require installation (more details later).
All results need to be copied to runtime/ folders for analysis.

### BMLP modules (BMLP-RMS & BMLP-SMP)

To run on datasets DG and DG+partial (Table 2 and Figure 4, 5): 
```commandline
cd BMLP/
bash run_cpu_exp.sh bmlp-rms full-5000 10
bash run_cpu_exp.sh bmlp-smp partial-5000 10
bash run_cpu_exp.sh bmlp-smp partial-range 10
```

Colab.ipynb benchmarks BMLP GPU and PyTorch implementation on DG, DG+partial and FB15K-237 [3] (Table 3):
```commandline
bash run_colab_exp.sh py-bmlp-gpu partial-5000 10
bash run_colab_exp.sh py-bmlp-gpu full-5000 10
bash run_colab_exp.sh py-bmlp-gpu FB15K 10
```

### Non-BMLP systems

To get runtime of non-BMLP systems SYSTEM_NAME in DATASET:
```commandline
bash run_cpu_exp.sh SYSTEM_NAME DATASET 10
```

DATASET options are:
- partial-range (DG+partial, varying node size)
- partial-5000  (DG+partial)
- full-5000     (DG)
- FB15K

SYSTEM_NAME options are:
- clg: Clingo [1] ([install](https://github.com/potassco/clingo/releases/))
- souffle: Souffle [2] ([install](https://souffle-lang.github.io/install))
- swipl: SWI-Prolog [4] ([install](https://www.swi-prolog.org/Download.html))
- bpl:   B-Prolog [5] (binary in experiments/bpl.zip)

### Experimental data and analysis
We provide a Jupyter notebook (scripts/analysis.ipynb) to compute statistical data and create figures.

Runtime results in the paper are stored at:
- experiments/path/full/runtime (runtime of BMLP-RMS and other systems)
- experiments/path/partial/runtime (runtime of BMLP-SMP and other systems)

## References

[1] M. Gebser, R. Kaminski, B. Kaufmann, and T. Schaub, ‘Clingo = ASP + Control: Preliminary Report’, Technical Communications of the Thirtieth International Conference on Logic Programming (ICLP’14), vol. 14, pp. 1–9, 2014.

[2] B. Scholz, H. Jordan, P. Subotić, and T. Westmann, ‘On fast large-scale program analysis in Datalog’, in Proceedings of the 25th International Conference on Compiler Construction, in CC 2016. New York, NY, USA: Association for Computing Machinery, Mar. 2016, pp. 196–206. doi: 10.1145/2892208.2892226.

[3] K. Toutanova and D. Chen, ‘Observed versus latent features for knowledge base and text inference’, in Proceedings of the 3rd Workshop on Continuous Vector Space Models and their Compositionality, A. Allauzen, E. Grefenstette, K. M. Hermann, H. Larochelle, and S. W. Yih, Eds., Beijing, China: Association for Computational Linguistics, 2015, pp. 57–66. doi: 10.18653/v1/W15-4007.

[4] J. Wielemaker, T. Schrijvers, M. Triska, and T. Lager, ‘SWI-Prolog’, Theory and Practice of Logic Programming, vol. 12, no. 1–2, pp. 67–96, 2012.

[5] N.-F. Zhou, ‘The language features and architecture of B-Prolog’, Theory and Practice of Logic Programming, vol. 12, no. 1–2, pp. 189–218, Jan. 2012, doi: 10.1017/S1471068411000445.

[6] S. H. Muggleton, 'Hypothesizing an algorithm from one example: the role of specificity', Philosophical Transactions of the Royal Society A: Mathematical, Physical and Engineering Sciences, 381(2251): 20220046, 2023.
