from wikidataintegrator import wdi_core


# def hello_world():
#     return(conn.search(index='bbn_co-031021_refgeo'))

# def index(indexin,idin,bodyin):    
#     return(conn.index(index=indexin, id=idin, body=bodyin))

# returns a dict w/ all info about a wd node (i.e. label, descr, properties/parent)
def get_wd_dict(qnode):
    return(wdi_core.WDItemEngine(qnode))

def get_wd_entity(qnode):
    try:
        entity_dict = wdi_core.WDItemEngine(qnode).get_wd_entity()
        rem_list = ['pageid', 'lastrevid', 'modified', 'descriptions', 'sitelinks']
        [entity_dict.pop(key) for key in rem_list]
        return entity_dict

    except:
        print("sorry...WDI can't find",qnode)
        return {}

def get_wd_parent(qnode_dict):
    return qnode_dict['claims']['P279']

# Return dict of results from SPARQL query
def sparql_query(propertyNode,qnode):
    try:
        return wdi_core.WDItemEngine.execute_sparql_query('SELECT ?childLabel WHERE { ?child wdt:%s wd:%s. SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }} LIMIT 10000'%(propertyNode,qnode))['results']['bindings']
        # wdi_core.WDItemEngine.execute_sparql_query('SELECT ?childLabel WHERE { ?child wdt:%s wd:%s. SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }} LIMIT 10000'%('P31','Q51591359'))['results']['bindings']
        # if len(results) <= 10000:
        #     return results
        # else:
        #     print("exceeded 100K results for query",qnode,propertyNode)
        #     return results[:1000]
    except Exception as err:
        print("sorry...WDI can't sparql",qnode,propertyNode, err)
        return []
