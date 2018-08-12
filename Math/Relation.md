# Relations

What is a Relation?

## Binary Relation: 

Sources: 
* [Wikipedia](https://en.wikipedia.org/wiki/Binary_relation)
* [Wolfram](http://mathworld.wolfram.com/Relation.html)

Any subset of a Cartesian product. 

The application appears to be that the pairs have a *relationship* rather than being randomly assigned to the pairing. The relationship is from a rule that may or may not be a function. 

## How to Read

We read ```x ~ y``` as: "```x``` is equivalent to ```y```"

## Physical Analogies

For sets A and B, a function could be assigned a rule which  moves items from A to B. So *movement* between sets would be the physical analogy to that situation. 

For relations, particularly the Equivalence Relation, the analogy is constrained to a single set. In this case, we are using some rule to establish an equivalence relation to partition a single set. 

### When pairs are from only A: 

If the pairs are only from A, they are a subset of the Cartesian product of A, written as: A^2 = A x A. The Cartesian product of A would be all possible pairs of A, so it stands to reason that some chosen set of pairs would be a subset, even an equal subset. 

### When pairs are from sets A and B: 

If the pairs are from both A and B, just like above, the list will be a subset of the Cartesian product of A and B, which is written as A x B. 

## Examples: 

### General: 

* Any random set of pairs
* a < b
* a divisible by b

### Domain-specific: 

* An [Adjacency Relation](http://mathworld.wolfram.com/AdjacencyRelation.html)
 is the list of Vertex to Vertex edges in a Graph, must be [irreflexive](http://mathworld.wolfram.com/Irreflexive.html) (which means that no Vertex may lead to itself) and [symmetric](http://mathworld.wolfram.com/Symmetric.html) (which means that every Vertex v to Vertex v' has a reverse pair v' to v). 

## Properties: 

Relations can have properties. The properties have names: 

* [Reflexive](http://mathworld.wolfram.com/Reflexive.html): A relation ```R``` on a set ```S``` is reflexive if ```x R x``` for every ```x``` in ```S```
* [Irreflexive](http://mathworld.wolfram.com/Irreflexive.html): Not Reflexive; A relation ```R``` on a set ```S``` is irreflexive if ```x R x``` for *no* ```x``` in ```S```
* [Symmetric](http://mathworld.wolfram.com/Symmetric.html): A relation ```R``` on a set ```S``` is symmetric if for every ```x``` and ```y``` in ```S``` we have ```x R y``` and ```y R x```
* [Anti-Symmetric](http://mathworld.wolfram.com/AntisymmetricRelation.html): A relation ```R``` on a Set ```S``` is anti-symmetric if distinct elements in the relation are not both related to each other. ?? They go on to say that "```x R y``` and ```y R x``` imply that ```x = y```" ...
* [Transitive](http://mathworld.wolfram.com/Transitive.html): A relation ```R``` on a Set ```S``` is transitive if for every ```x```, ```y``` and ```z``` in ```S``` such that ```x R y``` and ```y R z```, we also have ```x R z```

## Notes: 

The rule which generates the set of relations [is not necessarily a function](https://www.reddit.com/r/explainlikeimfive/comments/n9btw/eli5_what_makes_a_relation_a_function/). 

The example there is a rule which produces a set like: 
* 1 -> 2
* 1 -> 3

So you have two outputs for 1, meaning the rule is not a function because you can't have more than one output for an input. 

### 

### Synonyms:

*correspondence*, *dyadic relation* and *2-place* relation are synonyms for binary relation.

## Finitary Relation: 

https://en.wikipedia.org/wiki/Finitary_relation

## Equivalence Relation: 

https://en.wikipedia.org/wiki/Equivalence_relation
