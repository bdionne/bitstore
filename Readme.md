## bitstore -- A document based approach to ontology development

The goal of bitstore is a minimal environment for ontology development
that is document centered, implemented as a
[couchapp](http://github.com/couchapp/couchapp) in conjunction with a
slightly modified version of
[couchdb](http://github.com/bdionne/couchdb/tree/bitstore). 

### Concepts as Documents

Description logics have been very successful for organizing knowledge
in complex domains and building vocabularies and taxonomies in these
domains. Depending on one's viewpoint and the level of inferencing
needed, the state of the art is either
[OWL Lite](http://www.w3.org/TR/owl-ref/#OWLLite) or
[OWL-DL](http://www.w3.org/TR/2004/REC-owl-features-20040210/#s1.) 

One of the challenges in using DL in a complex domain is that one
typically needs a considerable amount of domain expertise in addition
to understanding the particular description logic used. Over the years we've seen that
modelers rarely use modifiers such as **all** and cardinalities,
instead typically opting for **some**. Mostly they want to build
taxonomies and have vague ideas about when to group role restrictions
and when a concepts restrictions are defining or not, in other words
both necessary and sufficient as opposed to just necessary.

Moreover, in building and maintaining large terminologies there are
concerns that have little to do with description logics, all the
normal IT issues such as workflow, revision history, auditing, and
business process rules. Taking a completely description logic centric
view in terms of system design makes handling these other, arguably
more pedestrian concerns, much more difficult.

This prototype attempts to take a bottoms up view of terminology by
treating a concept as a document. By document we mean a collection of
name value pairs that is uniquely identified, a couchdb document.

### Graphs and Names

### Primitive versus Defined

### All, Some, and Cardinality

### A simple triple store
