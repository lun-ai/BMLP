# Warning!!!  This is a sample interface, and should only serve as an
# illustration of how an interface might be written.  There are no known
# bugs, but it should be thoroughly tested.
#
# This is also not an example of writing good Python code.
#
# This interface works by translating the elements of an rdflib graph
# to prolog data structures.  Conceptually:#F
# First, any rdflib.Literal objects are translated to 2-tuples (URLRefs
# and Bnodes are unaffected)
# Second, each triple in the rdflib.Graph are translated to 3-tuples.
# Thus the graph is conveyed to Prolog as a list of 3-tuples, each of
# whose arguments may be a string or 2-tuple.
# When translating to Python from Prolog, literals, triples and a graph
# are created from lists of the above form.
 
# This has been tested out on N-triple and Turtle files.

from rdflib import *
from rdflib_hdt import HDTStore

# leaves non-literals unaffected.
def rdflib_term_to_tuple(o):
    if type(o) == Literal:
        if o.language:
            lang = o.language
        else:
            lang = ''
        if o.datatype:
            dt = o.datatype
        else:
            dt = ''
        onew = (str(o),dt,lang)
    else:
        onew = o
    return(onew)

def graph_to_tuples(gr):
    glist = []
    for edge in gr:
        elt_list = []
        for elt in edge:
            elt_new = rdflib_term_to_tuple(elt)
            elt_list.append(elt_new)
        glist.append(tuple(elt_list))
    return(glist)

def rdflib_parse(File,**Kwargs):
    g = Graph()
    g.parse(File,**Kwargs)
    return(graph_to_tuples(g))

def rdflib_parse_nquads(File,**Kwargs):
    g = ConjunctiveGraph()
    data = open(File,'rb')
    g.parse(data,format = 'nquads',**Kwargs)
    return(graph_to_tuples(g))

#----------------------------------------

# probably need to change
def is_prolog_blank_node(node):
    return(node[0] == '_')

def tuples_to_graph(inlist):
    g = Graph()
    for ntuple in inlist:
        tlist = []
        for elt in ntuple:
            if type(elt) == tuple:
                if elt[2] != '':
                    eltnew = Literal(elt[0],lang=elt[2])
                elif elt[1] != '':
                    eltnew = Literal(elt[0],datatype=elt[1])
                else:
                    eltnew = Literal(elt[0])
            else:
                if is_prolog_blank_node(elt):
                    eltnew = Bnode(elt)
                else:
                    eltnew = URIRef(elt)
            tlist.append(eltnew)
        newtup = tuple(tlist)
        g.add(newtup)
    return(g)

def rdflib_write_file_no_decode(Inlist, File,**Kwargs):
    g = tuples_to_graph(Inlist)
    with open(File,"w") as fp:
        print(g.serialize(**Kwargs),file = fp)

def rdflib_write_file(Inlist, File,**Kwargs):
    g = tuples_to_graph(Inlist)
    with open(File,"w") as fp:
        print(g.serialize(**Kwargs).decode("utf-8"),file = fp)

#------------------------------------------
# HDT

hdt_store = None
hdt_graph = None

def hdt_load(hdtFile):
    global hdt_store
    hdt_store = HDTStore(hdtFile)
    new_graph()
    return hdt_store

def new_graph():
    global hdt_graph
    hdt_graph = Graph(store=hdt_store)

import validators    
def hdt_query(Arg1,Arg2,Arg3):
    global hdt_graph
    Rarg1 = validate(Arg1)
    Rarg2 = validate(Arg2)
    Rarg3 = validate(Arg3)
#    print((Rarg1,Rarg2,Rarg3))
    It = hdt_graph.triples((Rarg1,Rarg2,Rarg3))
#    print("hdt finished")
#    print([trip for trip in It])
#    Sum = sum(1 for _ in It)
#    print(Sum)
    ret =  [rdf_triple_to_xsb(trip) for trip in It]
#    print("ret constructed")
    return ret

def rdf_triple_to_xsb(TripIn):
    Arg0 =  rdflib_term_to_tuple(TripIn[0])
    Arg1 =  rdflib_term_to_tuple(TripIn[1])		
    Arg2 =  rdflib_term_to_tuple(TripIn[2])
    return (Arg0,Arg1,Arg2)

            
def validate(Arg):
    if Arg == 'None':
        return None
    elif type(Arg) == tuple:
        if Arg[2] != '':
            eltnew = Literal(Arg[0],lang=Arg[2])
        elif Arg[1] != '':
            eltnew = Literal(Arg[0],datatype=Arg[1])
        else:
            eltnew = Literal(Arg[0])
        return eltnew
    elif validators.url(Arg):
        return URIRef(Arg)
    else:
        return Arg
