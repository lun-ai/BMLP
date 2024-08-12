from rdflib import Graph, URIRef
from rdflib.namespace import RDFS
from datasets import load_dataset, get_dataset_split_names

# triples: 881223 entities: 260023
predicate = 'neighbors'
relations = []
output_path = f'./experiments/YAGO/{predicate}.pl'
output_path_clg = f'./experiments/YAGO/{predicate}.lp'

def strip_object(str):
    return str.replace("<http://yago-knowledge.org/resource/", "").strip(">")

def extract_YAGO():
    # Load the YAGO dataset (only train split is available)
    YAGO = load_dataset('wikipunk/yago45en', num_proc=10, split='train')

    # Extract relations
    for triple in YAGO:
        if predicate in triple['predicate']:
            relations.append((predicate, triple['subject'], triple['object']))

    # write relations as datalog
    with open(output_path, "w") as output_file:
        for (p, c1, c2) in relations:
            output_file.write(f"{p}('{strip_object(c1)}', '{strip_object(c2)}').\n")

    print(f"Extracted {len(relations)} {predicate} relations and saved them to {output_path}.")

def YAGO_split():
    print(get_dataset_split_names('wikipunk/yago45en'))

if __name__ == "__main__":

    extract_YAGO()

    with open(output_path, "r") as datafile:
        with open(output_path_clg, "w") as output_file:
            for line in datafile:
                output_file.write(line.replace('\'','\"'))

    pass
