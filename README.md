Introduction
Welcome to the Racket Suffix Tree Assignment! This project is designed to help you understand and implement suffix trees (ST) in Racket, a powerful concept in string processing and data structures. Throughout this assignment, you will develop a library for constructing and manipulating suffix trees, which are useful for various text processing applications.

Assignment Overview
The assignment is divided into two main stages:

Stage 1: Basic Suffix Tree Operations
In this stage, you will familiarize yourself with the structure and representation of suffix trees. You will implement a set of functions to construct and manipulate suffix trees. This includes defining the tree structure, extracting branches, and matching patterns within the tree.

Stage 2: Suffix Tree Construction
In the second stage, you will focus on building suffix trees from given texts. You will implement constructors to create both atomic and compact suffix trees from the text. This involves extracting suffixes and constructing the tree incrementally.

Tasks
Here is a brief summary of the tasks you will need to complete for each stage:

Stage 1: Basic Suffix Tree Operations
Represent a suffix tree as a list of branches.
Implement functions such as first-branch, other-branches, get-branch-label, get-branch-subtree, get-ch-branch, longest-common-prefix, longest-common-prefix-of-list, match-pattern-with-label, and st-has-pattern?.
Stage 2: Suffix Tree Construction
Extract all suffixes of a given text using the get-suffixes function.
Construct the atomic suffix tree using the ast-func function.
Construct the compact suffix tree using the cst-func function.
