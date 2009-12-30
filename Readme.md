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
more pedestrian concerns, much more difficult. These concerns
are more than just normal IT issues. Terminologies are often built by
groups of experts from multiple domains, collaborating in a curation
process where multiple authors might work on overlapping areas of the
terminology. Conflict resolution becomes a process whereby
definitions evolve and are better defined over time. Again, taking a
completely DL centric view, which is what is done when OWL is used for
encoding these terminologies makes addressing these other concerns
more difficult. Having meta-level annotation properties that are
orthogonal to the semantics does little to mitigate this.

Bitstore is a prototype that will attempt to take a bottoms up view of terminology by
treating a concept as a document. By document we mean a collection of
name value pairs that is uniquely identified, to wit, a couchdb
document. CouchDB is schema-less and in many ways similar to other simple key-value stores
such as Berkeley DB. Of course in practice a group of
documents will have a schema but nothing is enforced. This enables
maximum flexibility in adding new slots and entails a minimal
committment to schema. Documents are uniquely identified which is
required to support multiple federated ontologies. Meaningless
identifiers could be used as document ids but one could also just add
them as a **code** slot and use CouchDB's ability to generate unique
ids internally.

### Description Logic

The basic terms in a description logic are called **concepts** or **classes** in OWL. Concepts are
defined in terms of other concepts, often called parents, and one can have roles or binary relations between concepts. For example a drug terminology may contain the concepts **anti-infective** and **oral** and have a relation such as **route-of-administration**. The concept **aspirin** may then be defined as:

    (define-concept aspirin (and anti-infective (all route-of-administration oral)))

The key idea is that a concept describes a set of individuals in some universe. Description logics can have quite a lot of features depending on whether they support disjunctions, disjointness, cardinalities and negations. The three main dialects of OWL, Lite, DL, and FULL, support increasing amounts of expressivity in terms of the logic with the tradeoff being the ability to do inferencing. OWL Full for example is largely intractable. 

By restricting the logic to a small set of features, description logics such as Ontylog have been able to support very large vocabularies, classifying them in reasonable time. The features of Ontylog can be encoded as conjunctions of terms whic are essentially binary predicates or triples. In fact OWL itself can be serialized as RDF so that triples are sufficent as storage for description logics.

### Simple Triple Store

Bitstore is taking a bottoms up approach to DL by starting with a simple triple store for each couchdb database. Each record is a triplet of couchdb document ids. So using the example above one might have triples:

    <aspirin, isa, anti-infective>
    <aspirin, rout-of-administration oral>

where all term with the same subject are implicitly conjoined together as part of a concept. Notice that **isa** and **route-of-administration** are relations and in general will connect multiples pairs of documents, nevertheless each relation will have a document associated with it, where we might store additional information such as whetehr the relation is transitive or has a converse

### Directed Acyclic Graphs

It's fairly easy to view a triple store as a labelled graph, where each triple <source,arrow,target> represents a labelled arc connecting two nodes in the graph. In descriptions logics like Ontylog, classification, the process of computing the subsumption relation, is an algorithm that takes the concepts and builds a directed acyclic graph using the subsumption relation. But one can imagine forming graphs of documents in general, regardless of any logic, so the approach will be to layer the implementatin of ontylog on top of a more general graph data structure persisted in a triple store. We think this will add a lot of schema like functionality back into couchdb in a principled way. Users might wish to relate any documents to one another. So basically:

   bitstore =  couchdb + graph database + FTI

We think this provides an excellent foundation for ontology development. Couchdb's schema-less design coupled with replication and the simplicity of the REST/JSON api will provide an architecturally simple, flexible, and scalable approach to building environments for multiple collaborators to develop ontologies.



