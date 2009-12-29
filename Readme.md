## Bitstore -- A document based approach to ontology development

The goal of bitstore is a minimal environment for ontology development
that is document centered, implemented as a
[couchapp](http://github.com/couchapp/couchapp) in conjunction with a
slightly modified version of
[couchdb](http://github.com/bdionne/couchdb/tree/bitstore). 

### Concepts as Documents

Description logics (DL) have been very successful for organizing knowledge
in complex domains and building vocabularies and taxonomies in these
domains. Depending on one's viewpoint and the level of inferencing
needed, the state of the art is either
[OWL Lite](http://www.w3.org/TR/owl-ref/#OWLLite) or
[OWL-DL](http://www.w3.org/TR/2004/REC-owl-features-20040210/#s1.) 

One of the challenges in using DL in a complex domain is that one
typically needs a considerable amount of domain expertise in addition
to understanding the particular description logic used. Over the years we've seen that
modelers rarely use modifiers such as **all** and **cardinalities**,
instead typically opting for **some**. Mostly they want to build
taxonomies and have vague ideas about when to group role restrictions
and when a concept's restrictions are defining or not, in other words
both necessary and sufficient as opposed to just necessary. The
aversion to the use of the **all** modifier seems to stem from the
counterintuitive nature of **all** being vacuously true and a
reluctance to make strong assertions. 

Moreover, in building and maintaining large terminologies there are
concerns that have little to do with description logics, all the
normal IT issues such as workflow, revision history, auditing, and
business process rules. Taking a completely description logic centric
view in terms of system design makes handling these other, arguably
more pedestrian concerns, much more difficult. Arguably these concerns
are more than just normal IT issues. Terminologies are often built by
groups of experts from multiple domains, collaborating in a curation
process where multiple authors might work on overlapping areas of the
terminology so that conflict resolution becomes a process whereby
definitions evolve and are better defined over time. Again, taking a
completely DL centric view, which is what is done when OWL is used for
encoding these terminologies makes addressing these other concerns
more difficult and having meta-level annotation properties that are
orthogonal to the semantics does little to mitigate this.

This prototype attempts to take a bottoms up view of terminology by
treating a concept as a document. By document we mean a collection of
name value pairs that is uniquely identified, to wit, a couchdb
document. CouchDB is schema-less. Of course in practice a group of
documents will have a schema but nothing is enforced. This enables
maximum flexibility in adding new slots and entails a minimal
committment to schema. Documents are uniquely identified which is
required to support multiple federated ontologies. Meaningless
identifiers could be used as document ids but one could also just add
them as a **code** slot and use CouchDB's ability to generate unique
ids internally.

### A simple triple store

Inferencing is supported in bitstore via a triple store. Since
OWL is serialized as RDF it's fairly straightforward to see the
connection, however bitstore is going to at most only support a
limited amount of expressivity in terms of DL features, making the
tradeoff in favor of scalability. Each triple is a triple of document ids.



### Graphs and Names

### Primitive versus Defined

### All, Some, and Cardinality


