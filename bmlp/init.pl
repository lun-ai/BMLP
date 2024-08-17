%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Path initilisation
%	Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic srcPath/1,lm_status/1.

init :- init(_).
init(Path) :- var(Path),!,
              retractall(srcPath(_)),
              assertz(srcPath('./temp/')),
              assertz(lm_status(initialised)).
init(Path) :-
              (exists_directory(Path) -> true;make_directory(Path)),
              atom_concat(Path,'/',Path1),
              retractall(srcPath(_)),
              assertz(srcPath(Path1)),
              assertz(lm_status(initialised)).