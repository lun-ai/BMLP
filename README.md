# BMLP
Boolean Matrix Logic Programming

## Using BMLP modules

src/

## Tests

## Evaluation

```bash run_exp.sh system_name experiments/networks/connect sub_folder```

Alternative systems are needed for comparison: 
- bpl:   B-Prolog (binary)
- xsbpl: XSB-Prolog (binary, can re-make if not usable by following the XSB/doc/userman/manual1.pdf)
- swipl: SWI-Prolog (Install from https://www.swi-prolog.org/Download.html)
- 

### Experimental data

CPU runtime of BMLP-RMS and other systems:
experiments/connect/full/runtime

CPU runtime of BMLP-SMP and other systems:
experiments/connect/partial/runtime