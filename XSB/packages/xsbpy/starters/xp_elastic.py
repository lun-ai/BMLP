from elasticsearch import Elasticsearch

conn = Elasticsearch(['localhost:9200'])

print(type(conn))

def hello_world():
    return(conn.search(index='bbn_co-031021_refgeo'))

def index(indexin,idin,bodyin):    
    return(conn.index(index=indexin, id=idin, body=bodyin))

def get(indexin,idin):
    return(conn.get(index=indexin, id=idin))

def refresh(indexin):
    return(conn.indices.refresh(index=indexin))

def search(indexin,bodyin):
    return(conn.search(index=indexin, body=bodyin))

def msearch(indexin,bodyin):
    return(conn.msearch(index=indexin, body=bodyin))
           
def get_indices():
    return(conn.indices.get_alias('*'))

def delete_index(indexin):
    return(conn.indices.delete(index=indexin))
