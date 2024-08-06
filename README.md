# BMLP
Boolean Matrix Logic Programming

## Using BMLP modules

src/

## Tests

## Evaluation

```bash run_exp.sh SYSTEM_NAME experiments/networks/connect/SUB_FOLDER REP```

Alternative systems are needed for comparison (system_alias: system and installation):
- all:   run all
- bpl:   B-Prolog (binary in BProlog/)
- xsbpl: XSB-Prolog (binary in XSB/bin, can re-make if not usable by following the XSB/doc/userman/manual1.pdf)
- swipl: SWI-Prolog (Install from https://www.swi-prolog.org/Download.html)
- clingo: Clingo (install from https://github.com/potassco/clingo/releases/)
- souffle: Souffle (binary in Souffle/ or following instructions from https://souffle-lang.github.io/build)

### Experimental data

CPU runtime of BMLP-RMS and other systems:
experiments/connect/full/runtime

CPU runtime of BMLP-SMP and other systems:
experiments/connect/partial/runtime