%% Your task is determine the RNA complement of a given DNA sequence.
%% Both DNA and RNA strands are a sequence of nucleotides.
%%
%% The four nucleotides found in DNA are adenine (A), cytosine (C), 
%% guanine (G) and thymine (T).
%%
%% The four nucleotides found in RNA are adenine (A), cytosine (C), 
%% guanine (G) and uracil (U).
%%
%% Given a DNA strand, its transcribed RNA strand is formed by replacing 
%% each nucleotide with its complement:
%%
%% G -> C
%% C -> G
%% T -> A
%% A -> U

-module(dna).
-export([dna/1]).

dna([])      -> {error, "empty list"};

dna(["G"]) -> ["C"];
dna(["C"]) -> ["G"];
dna(["T"]) -> ["A"];
dna(["A"]) -> ["U"];

dna(["G"|T]) -> ["C"|dna(T)];
dna(["C"|T]) -> ["G"|dna(T)];
dna(["T"|T]) -> ["A"|dna(T)];
dna(["A"|T]) -> ["U"|dna(T)].
