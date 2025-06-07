import time
import torch
import pandas as pd
import BMLP_GPU.bmlp.core.tensor as bmlp_tensor
from BMLP_GPU.bmlp.core.utils import *

if __name__ == "__main__":

    path_dir = 'experiments/path/partial'
    unary = pd.read_csv(path_dir + '/unary_relations.csv')
    binary = pd.read_csv(path_dir + '/binary_relations.csv')
    data1 = create_matrices_from_relations('edge',
                                           ['node', 'node'],
                                           unary.values.tolist(),
                                           binary.values.tolist())

    m1 = data1['matrix']

    device = torch.cuda.current_device()
    v = torch.zeros(1, m1.shape[0], dtype=bmlp_tensor.D_TYPE, device=device)
    v[0, 0] = 1.0
    m1 = torch.tensor(m1, dtype=bmlp_tensor.D_TYPE, device=device)
    start = time.time()
    m3 = bmlp_tensor.SMP(v, m1)
    end = time.time()
    print(end - start)
