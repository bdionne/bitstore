## A graph based approach to description logic

This is a new implementation of ontylog, well about 75% of the features of ontylog. Historically ontylog differs from other systems such as Pellet and Fact in that the approach to classification is based on graph walking and is structural or intensional, rather than based on tableaux techniques using models and finite failure. The tradeoff is limited expressivity in favor of scalability. To this day this author does not believe that tableaux based classifiers come anywhere near ontylog in performance and scalability. [NCI Thesaurus](http://www.cancer.gov/cancertopics/terminologyresources) can take 40 minutes of so for the initial classification using Pellet, where ontylog could do it in well under 3 minutes, most of that time spent talking to a relational database. Moreover Pellet uses much more memory. However it's important to emphasize that the comparison is apples and oranges as ontylog has far fewer features than Pellet, which supports all of OWL-DL.

### Ontylog

Ontylog descended from K-Rep, a DL built at IBM in the early 90s, originally on the symbolics lisp machine. At the time, [Bernhard Nebel's](http://books.google.com/books?id=d-fTkUb4FfwC&printsec=frontcover&dq=DL+Bernhard+Nebel&source=gbs_similarbooks_r&cad=2#v=onepage&q=&f=false) thesis work had begun formalizing these systems as logics, and semantics and the complexity of various constructs were becoming known. Deborah McGuiness wrote a good [history](http://www-ksl.stanford.edu/people/dlm/papers/dls-emerge-final.doc) of this period that provides a sense of how many languages there were.

Ontylog was designed with scalability in mind, willingly sacrificing expressivity. In particular it does not support negation and disjunction, two features that are arguably computationally expensive. We've also found that in practice they are also difficult to use in modeling and knowledge engineering. Note that this observation extends to all of OWL-DL. We feel that the use of OWL as a foundation for the semantic web is flawed and will continue to be a technology of the future. We recognize that this is a contentious and strong position and we mean it with all due respect and affection. We hope that this new approach to ontylog will show this position to be more than just an opinion and will incorporate all the best of DL and lessons learned in the past. Ontylog added a few things, motivated by performance and practical concerns, such as role groups, right identities and kinds. For example the use of kinds enables a taxonomy to be split into smaller sets of concepts that can be classified separately, thereby improving performance in certain scenarios. Right identities enabled the mixing of **isa** and **part-of** relations in a principled way so that one could infer that a disease of the myocardium is a disease of the heart.

Classification algorithms, which compute the subsumption relation, as implemented in ontlyog, involve walking around in a directed acyclic graph. So when a relational database is used for persistence the O-R mapping problem rears it's ugly head. The key to solving this was to recognize that much of the character data relating to terms, annotation properties, names, etc.. were not needed for classification. So the classification graphs of concepts could be serialized and stored in simple relational tables. 

For various practical reasons touched on in the [overview](http://github.com/bdionne/bitstore/blob/bitcask/Readme.md) of bitstore, this new implementation is going to take a further step and completely separate the logical inferencing from the management of the terminologies by treating concepts as simply independent documents. This will enable different reasoners to be used for each database.

### Can a key-value store like bitcask be used to store graphs efficiently

One initial prototype made use of erlangs awesome process model and created a pid for each node in the classification graph. This [version](http://github.com/bdionne/bitstore/tree/dagsaspids) was interesting in that a graph could be pretty large as long as the heap of each node stayed small. For typical ontologies this worked well. However to support multiple databases concurrently it didn't seem like it would sacle and also one has to be careful using pids to keep the messages separate. Mnesia was also used as the underlying store which is ideal for something as simple as a triple store.

This version is going to use [Bitcask](http://github.com/basho/bitcask.git) as a backing store. We're also using it for the inverted index in the FTI piece of bitstore so it would be simpler if it could work for the graphs as well. 

This work is very much a prototype and the design will likely evolve considerably over time, .eg. mnesia seems to have it's share of critics, but is perfectly adequate for prototyping.




