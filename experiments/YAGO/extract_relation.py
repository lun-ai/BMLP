from rdflib import Graph, URIRef
from rdflib.namespace import RDFS
from datasets import load_dataset

if __name__ == "__main__":

    # Load the YAGO dataset
    YAGO = load_dataset('wikipunk/yago45en', num_proc=4, split='train')

    # Extract subClassOf relations
    first_row = YAGO[0]

    print(first_row)

    # # write relations as datalog
    # with open('./experiments/YAGO/subclassof.pl', "w") as output_file:
    #     for c1, c2 in subclass_relations:
    #         output_file.write(f"subClassOf({c1}, {c2}).\n")