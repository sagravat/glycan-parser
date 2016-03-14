# Glycan Parser

The glycan-parser is a utility library that converts an IUPAC or linear code glycan structure to a Glycan object which can used for searching and
manipulating the entire glycan structure or sub structure. This library uses the Scala parser combinator library to parse the glycan structure into a **Glycan** object. The grammar for the parser is described in **IupacParser** which includes the EBNF like language for the 
    *   Monosaccharides
    *   Linkages
    *   Anomers
    *   Linker Arm
    *   Branches


