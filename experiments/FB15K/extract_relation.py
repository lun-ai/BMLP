# Define the file path to the FB15k dataset.
# 7991 triples, 14541 objects
fb15k_file_paths = ['train.txt', 'valid.txt', 'test.txt']

relation1 = '/location/location/contains'
relation2 = '/location/location/adjoin_s./location/adjoining_relationship/adjoins'

# Initialize a list to store the filtered triples.
triples1 = []
triples2 = []
objects = []

# Open the dataset file and parse it line by line.
for f in fb15k_file_paths:
    with open(f, 'r') as file:
        for line in file:
            c1, predicate, c2 = line.strip().split('\t')
            objects.append(c1)
            objects.append(c2)
            if predicate == relation1:
                triples1.append(('contains', c1, c2))
            elif predicate == relation2:
                triples2.append(('adjoins', c1, c2))

# Define the output file path for the parsed triples.
output_file_path = 'background.pl'
objects = list(dict.fromkeys(objects))
triples = triples1 + triples2

# Write the filtered triples to the output file.
with open(output_file_path, 'w') as output_file:
    for triple in triples:
        output_file.write(triple[0] + '(\'' + triple[1] + '\',\'' + triple[2] + '\').' + '\n')
    for obj in objects:
        output_file.write(f"location('{obj}').\n")

# Print a summary of the results.
print(f"Extracted {len(triples)} triples, {len(objects)} objects and saved them to {output_file_path}.")
