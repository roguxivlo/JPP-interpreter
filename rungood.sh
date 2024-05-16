#!/bin/bash

# Define the path to the Interpreter executable
INTERPRETER="./Interpreter"

# Directory containing files to process
FILES_DIRECTORY="good/"

# Output directory
OUTPUT_DIRECTORY="output/"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIRECTORY"

# Loop through each file in the directory
for file in "$FILES_DIRECTORY"*
do
    # Extract filename without path and extension
    filename=$(basename -- "$file")
    filename_no_ext="${filename%.*}"

    # Run the Interpreter executable on the file, capturing output and errors
    "$INTERPRETER" "$file" > "$OUTPUT_DIRECTORY$filename_no_ext.out" 2> "$OUTPUT_DIRECTORY$filename_no_ext.err"

    # Print status message
    echo "Processed $file"
done

echo "Processing complete."
