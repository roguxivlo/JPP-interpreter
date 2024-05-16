#!/bin/bash

# Define the path to the Interpreter executable
INTERPRETER="./Interpreter"

# Directory containing files to process
GOOD_FILES_DIRECTORY="good/"
BAD_FILES_DIRECTORY="bad/"

# Output directory
GOOD_OUTPUT_DIRECTORY="good/output/"
BAD_OUTPUT_DIRECTORY="bad/output/"

# Create output directory if it doesn't exist
mkdir -p "$GOOD_OUTPUT_DIRECTORY"
mkdir -p "$BAD_OUTPUT_DIRECTORY"

echo "Processing good files..."
# Loop through each file in the directory
for file in "$GOOD_FILES_DIRECTORY"*
do
    # Extract filename without path and extension
    filename=$(basename -- "$file")
    filename_no_ext="${filename%.*}"

    # if file is a directory, skip it
    if [ -d "$file" ]; then
        continue
    fi
    # Run the Interpreter executable on the file, capturing output and errors
    "$INTERPRETER" "$file" > "$GOOD_OUTPUT_DIRECTORY$filename_no_ext.out" 2> "$GOOD_OUTPUT_DIRECTORY$filename_no_ext.err"

    # Print status message
    echo "Processed $file"
done

echo "Processing bad files..."
for file in "$BAD_FILES_DIRECTORY"*
do
    # Extract filename without path and extension
    filename=$(basename -- "$file")
    filename_no_ext="${filename%.*}"
    if [ -d "$file" ]; then
        continue
    fi
    # Run the Interpreter executable on the file, capturing output and errors
    "$INTERPRETER" "$file" > "$BAD_OUTPUT_DIRECTORY$filename_no_ext.out" 2> "$BAD_OUTPUT_DIRECTORY$filename_no_ext.err"

    # Print status message
    echo "Processed $file"
done

echo "Processing complete."
