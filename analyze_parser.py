#!/usr/bin/env python3
"""
Script to help migrate parser from stub emit_operations to real LLVM codegen
"""

import re
import sys

def analyze_parser(filename):
    """Analyze the parser to find all emit_operation calls"""
    with open(filename, 'r') as f:
        content = f.read()
    
    # Find all emit_operation calls
    pattern = r'emit_operation\([^)]+\);'
    matches = re.findall(pattern, content)
    
    print(f"Found {len(matches)} emit_operation calls")
    print("\nExamples:")
    for i, match in enumerate(matches[:10]):
        print(f"{i+1}. {match}")
    
    # Find all gen_temp() calls
    temp_pattern = r'gen_temp\(\)'
    temp_matches = re.findall(temp_pattern, content)
    print(f"\nFound {len(temp_matches)} gen_temp() calls")
    
    # Find stub functions
    stub_pattern = r'void emit_operation.*?\n.*?\n.*?\n\}'
    stub_matches = re.findall(stub_pattern, content, re.DOTALL)
    print(f"\nFound {len(stub_matches)} stub function definitions")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        analyze_parser(sys.argv[1])
    else:
        print("Usage: analyze_parser.py <parser.y>")
