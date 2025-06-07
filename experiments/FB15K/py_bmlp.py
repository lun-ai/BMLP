import time
import torch
import pandas as pd
import BMLP_GPU.bmlp.core.tensor as bmlp_tensor
# import BMLP_GPU.bmlp.core.matrix as bmlp_matrix
# import graphblas as gb
from BMLP_GPU.bmlp.core.utils import *

if __name__ == "__main__":

    fb15k_dir = 'experiments/FB15K'
    unary = pd.read_csv(fb15k_dir + '/unary_relations.csv')
    binary = pd.read_csv(fb15k_dir + '/binary_relations.csv')
    data1 = create_matrices_from_relations('contains',
                                           ['location', 'location'],
                                           unary.values.tolist(),
                                           binary.values.tolist())
    data2 = create_matrices_from_relations('adjoins',
                                           ['location', 'location'],
                                           unary.values.tolist(),
                                           binary.values.tolist())
    m1 = data1['matrix']
    m2 = data2['matrix']

    device = torch.cuda.current_device()
    m1 = torch.tensor(m1, device=device)
    m2 = torch.tensor(m2, device=device)
    start = time.time()
    m3 = bmlp_tensor.RMS(m1, m2)
    mt3 = bmlp_tensor.transpose(m3)
    mit3 = bmlp_tensor.addI(mt3)
    mt2 = bmlp_tensor.transpose(m2)
    m4 = bmlp_tensor.add(m2, mt2)
    m5 = bmlp_tensor.mul(mit3, m4)
    mn5 = bmlp_tensor.negate(m5)
    end = time.time()
    print(end - start)
