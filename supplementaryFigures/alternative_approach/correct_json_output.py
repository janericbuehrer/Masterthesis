import json

# Function to process each line and convert it to the desired JSON format
def process_line(line, line_number):
    # Strip leading and trailing whitespace from the line
    line = line.strip()
    
    # Check if the line is empty or contains only whitespace characters
    if not line:
        print(f"Skipping empty line {line_number}")
        return None
    
    # Split the line by tab to separate the 'Iteration' column
    parts = line.split('\t')
    
    if len(parts) != 2:
        print(f"Issue with line {line_number}: Line does not have two tab-separated values.")
        return None
    
    try:
        # Extract the stoch_map part
        stoch_map = json.loads(parts[1])

        return stoch_map
    except json.JSONDecodeError as e:
        print(f"Issue with line {line_number}: JSONDecodeError - {e}")
        return None

# Path to your input and output files
input_file = '/home/buehrerj/gentiana/output/biome/m8/biome_stoch_8_json.log'
output_file = '/home/buehrerj/gentiana/output/biome/m8/biome_stoch_8_corrected.json'

# Read input file, process each line, and write to output JSON file
with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
    # Write the starting bracket of JSON object
    outfile.write('{\n')
    outfile.write('"stoch_map": [\n')
    
    # Process each line
    first_line = True
    for line_number, line in enumerate(infile, start=1):
        # Process the line
        stoch_map = process_line(line, line_number)
        
        if stoch_map:
            if not first_line:
                outfile.write(',\n')  # Add comma if it's not the first line
            else:
                first_line = False
            
            # Write the processed data to output file
            json.dump(stoch_map, outfile, indent=2)
        
    # Write the closing bracket for JSON array
    outfile.write('\n]\n}')
