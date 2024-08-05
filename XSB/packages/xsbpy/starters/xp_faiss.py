# Written by Albert Ki

import faiss
import numpy as np
import xp_faiss_numpy

faissIndex = 0

def create_index_from_file(xb_file,d):
    """Faiss is built around the Index object which encapsulates the 
    database vectors. Use the simplest index that performs brute-force 
    L2 distance search on the database vectors.

    :param xb_file (str): text file containing nb rows of d-length database 
    vectors that must be indexed, and that we are going to search in
    :param d (int): dimensionality of database vectors
    :return (faiss.IndexFlatL2): the built faiss index
    """
    global faissIndex
    xb = np.loadtxt(xb_file,dtype='float32')
    faissIndex = faiss.IndexFlatL2(d)   # build the index
    # print(index.is_trained)
    faissIndex.add(xb)                  # add vectors to the index
    # print(index.ntotal)
    return faissIndex

def create_index(xb,d):
    """Faiss is built around the Index object which encapsulates the 
    database vectors. Use the simplest index that performs brute-force 
    L2 distance search on the database vectors.

    :param xb (numpy array): nb-by-d matrix of database vectors that must be 
    indexed, and that we are going to search in
    :param d (int): dimensionality of database vectors
    :return (faiss.IndexFlatL2): the built faiss index
    """
    index = faiss.IndexFlatL2(d)   # build the index
    # print(index.is_trained)
    index.add(xb)                  # add vectors to the index
    # print(index.ntotal)
    return index

def search(index,xq,k):
    """For each query vector, find its k nearest neighbors in the database.
    Return a nq-by-k matrix of distances.
    
    :param xq (numpy array): nq-by-d query vectors, for which we need to find 
    k nearest neighbors
    :param k : number of nearest neighbors to search
    :return D (numpy array): nq-by-k floating-point matrix with the corresponding 
    squared distances
    :return I (numpy array): nq-by-k integer matrix, where row i contains the IDs 
    of the neighbors of query vector i, sorted by increasing distance
    """
    global faissIndex
#    print('search1')
#    print(index)
#    print(xq)
    dists_xb, indexes_xb = faissIndex.search(xq, k)     # actual search
#    print('search2')
    # print(dists_xb[:5])
    # print(I[:5])                   # neighbors of the 5 first queries
    # print(I[-5:])                  # neighbors of the 5 last queries
    # return (dists_xb,indexes_xb)
    return dists_xb,indexes_xb

def get_k_nearest_neighbors_to_node(node,k):
    global faissIndex
    vec = xp_faiss_numpy.xq[node]
    dists_xb, indexes_xb = search(faissIndex,np.array([vec]),k)
    return (dists_xb[0].tolist(),indexes_xb[0].tolist())

def get_threshold_neighbors_to_node(node,distance_threshold):
    dists_xb, indexes_xb = get_k_nearest_neighbors_to_node(node,len(xp_faiss_numpy.xq))
    dists_xb = np.array(dists_xb)
    dists_xb = dists_xb[dists_xb <= distance_threshold]
    indexes_xb = indexes_xb[:len(dists_xb)]
    return (dists_xb.tolist(),indexes_xb)

