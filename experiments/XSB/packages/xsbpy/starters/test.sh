
XSB='../../../bin/xsb'

./gentest.sh "$XSB" xp_rdflib "test_nt." xp_rdflib_nt
./gentest.sh "$XSB" xp_rdflib "test_ttl." xp_rdflib_ttl
# writing seems to mess up on blank nodes.
#./gentest.sh "$XSB" xp_rdflib "test_nq." xp_rdflib_nq
