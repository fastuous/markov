# Markov Chain Text Generation

Spec found [here](https://trumandeyoung.com/markov/spec.pdf)

This is a weekend project whose purpose was to take a Java assignment in the University of New Mexico Computer Science
undergraduate curriculum and rewrite it in Haskell.

The first iteration (pre- git repository unfortunately) used lazy string IO and simple string manipulation instead of 
parsing to build the markov chain data structure. This version was relatively inefficient (required 3 minutes on my desktop
to parse Moby Dick). I then decided to refactor/rewrite to take advantage of a more efficient Text parser to build the
markov chain. 

An explanation of some design choices:
  1. I used Attoparsec because I like the simplicity of fully-backtracking parsers and didn't need the error-reporting
features that other parsers have to offer.
  2. The spec suggested padding the beginning and end of input text with *order* number of nonword tokens. I chose the character
  `\NUL` to treat as my nonword token whether I was parsing words or single characters.
  3. While using Text to do all my IO and parsing *significantly* improved runtime (less than 1sec down from over 3minutes), the
  necessary refactoring and expansion led to slightly longer, less readable code. More experience with Text might be needed to cut
  down on this effect.
  
This project can be built with the usual [stack commands](https://docs.haskellstack.org/en/stable/README/#quick-start-guide), and then
run in the same way outlined in the spec. Some example source materials can be found in the `texts` folder.
