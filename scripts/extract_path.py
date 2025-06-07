# Define the file path to the FB15k dataset.
# 7991 triples, 14541 objects
from BMLP_GPU.bmlp.core.utils import save_relations_to_csv
import argparse


if __name__ == "__main__":

    # obtain the src_path from arguments
    parser = argparse.ArgumentParser(
        description='Extract relations from DG/DG-partial dataset.')
    parser.add_argument('--src_path', type=str, required=True,
                        help='Path to the DG/DG-partial dataset directory.')
    args = parser.parse_args()
    src_path = args.src_path

    # Read the background.pl file
    background_path = src_path + '/background.pl'
    pairs = []
    triples = []

    with open(background_path, 'r') as file:
        for line in file:
            line = line.strip()
            if line.startswith('edge('):
                # Parse contains relation: contains('entity1','entity2').
                parts = line[len('edge('):-2].split(',')
                entity1 = parts[0].strip("'")
                entity2 = parts[1].strip("'")
                triples.append(('edge', entity1, entity2))
            elif line.startswith('node('):
                # Parse location unary relation: location('entity').
                entity = line[len('node('):-2].strip("'")
                pairs.append(('node', entity))

    # Print a summary of the triples extracted.
    print(
        f"Extracted {len(triples)} triples, {len(pairs)} objects and saved them to {src_path}.")

    save_relations_to_csv(pairs, triples, dir_path=src_path)
