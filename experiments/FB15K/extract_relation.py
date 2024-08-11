# Define the file path to the FB15k dataset.
# fb15k_file_paths = ['freebase_mtr100_mte100-train.txt', 'freebase_mtr100_mte100-valid.txt',
#                     'freebase_mtr100_mte100-test.txt']  # Replace with the actual path to the FB15k dataset
fb15k_file_paths = ['train.txt','valid.txt','test.txt']

# Define the relation for `isLocatedIn`.
relation = '/location/location/contains'
# relation = '/people/person/parents'

# Initialize a list to store the filtered triples.
is_located_in_triples = []

# Open the dataset file and parse it line by line.
for f in fb15k_file_paths:
    with open(f, 'r') as file:
        for line in file:
            c1, predicate, c2 = line.strip().split('\t')
            if predicate == relation:
                is_located_in_triples.append(('contains', c1, c2))

# Define the output file path for the parsed triples.
output_file_path = 'contains.pl'

# Write the filtered triples to the output file.
with open(output_file_path, 'w') as output_file:
    for triple in is_located_in_triples:
        output_file.write(triple[0] + '(\'' + triple[1] + '\',\'' + triple[2] + '\').' + '\n')

# Print a summary of the results.
print(f"Extracted {len(is_located_in_triples)} 'isLocatedIn' triples and saved them to {output_file_path}.")
