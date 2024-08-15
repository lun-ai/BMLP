# Define the file path to the FB15k dataset.
# 7991 triples, 14541 objects

src_path = 'experiments/FB15K/'
fb15k_file_paths = [src_path + 'train.txt', src_path + 'valid.txt', src_path + 'test.txt']

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
output_file_path1 = src_path + 'background.pl'
output_dl_path1 = src_path + 'CONTAINS.facts'
output_dl_path2 = src_path + 'ADJOINS.facts'
output_dl_path3 = src_path + 'location.facts'
output_file_path2 = src_path + 'background.lp'
objects = list(dict.fromkeys(objects))
triples = triples1 + triples2

# Write the filtered triples to the output file.
with open(output_file_path1, 'w') as output_file:
    for triple in triples:
        output_file.write(triple[0] + '(\'' + triple[1] + '\',\'' + triple[2] + '\').' + '\n')
    for obj in objects:
        output_file.write(f"location('{obj}').\n")

with open(output_file_path2, 'w') as output_file:
    for triple in triples:
        output_file.write(triple[0] + '(\"' + triple[1] + '\",\"' + triple[2] + '\").' + '\n')
    for obj in objects:
        output_file.write(f"location(\"{obj}\").\n")

with open(output_dl_path1, 'w') as output_file:
    for triple in triples1:
        output_file.write(f"\"{triple[1]}\"" + '\t' + f"\"{triple[2]}\"" + '\n')
with open(output_dl_path2, 'w') as output_file:
    for triple in triples2:
        output_file.write(f"\"{triple[1]}\"" + '\t' + f"\"{triple[2]}\"" + '\n')
with open(output_dl_path3, 'w') as output_file:
    for obj in objects:
        output_file.write(f"\"{obj}\"\n")

# Print a summary of the results.
print(f"Extracted {len(triples)} triples, {len(objects)} objects and saved them to {output_file_path1}.")
