/*
TES: This code is a tool to help with binary searches.  You give
binSearch_version a subversion revision that is known to be good, and
another that is known to be bad, and it checks out and makes an XSB
for the version whose number splits the interval between from and to.

Its a work in progress, and isn't recursive, but it does speed up the
process.
*/


:- import concat_atom/2 from string.
:- import append/3 from basics.
:- import shell/2 from shell.

binSearch_version(From,To):-
    Middle is From + floor((To - From)/2),
    make_version(Middle).
	
make_version(Vnum):-
    concat_atom(['xsb-',Vnum],Dirname),
    exec_command(['mkdir ',Dirname]),
    cd(Dirname),
    number_codes(Vnum,L),atom_codes(Vstr,L),
    exec_command(['svn checkout -r ',Vstr,' https://svn.code.sf.net/p/xsb/src/trunk xsb-src']),
    cd('xsb-src/XSB/build'),
    writeln(cded),
    exec_command('./configure'),
    exec_command('./makexsb').
    
exec_command(Cmd):-
    shell(Cmd,Err),
    (Err == 0 ->
        true
    ;  (is_list(Cmd) ->
	   append(Cmd,[' returned err ',Err],AbortMsg)
	 ; append([Cmd],[' returned err ',Err],AbortMsg) ) ).

end_of_file.
    


cd xsb-src/XSB/build/
./configure
./makexsb
