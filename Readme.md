## Bitstore -- A document based approach to ontology development

The goal of bitstore is a minimal environment for ontology development
that is document centered, implemented as a
[couchapp](http://github.com/couchapp/couchapp) in conjunction with a
slightly modified version of
[couchdb](http://github.com/bdionne/couchdb/tree/bitstore). 

**Note:** These notes are rough thoughts that I'll be developing in parallel with the code.
They assume a lot of background information and are in no way self-contained.

### Concepts as Documents

Description logics (DL) have been very successful for organizing knowledge
in complex domains and building vocabularies and taxonomies in these
domains. Depending on one's viewpoint and the level of inferencing
needed, the state of the art is either
[OWL Lite](http://www.w3.org/TR/owl-ref/#OWLLite) or
[OWL-DL](http://www.w3.org/TR/2004/REC-owl-features-20040210/#s1.) 

One of the challenges in using DL in a complex domain is that one
typically needs a considerable amount of domain expertise in addition
to understanding the particular description logic being used. Over the years we've seen that
modelers rarely use modifiers such as **all** and **cardinalities**,
instead typically opting for **some** if not explicitly then certainly implicitly. Mostly they want to build
taxonomies and have vague ideas about when to group role restrictions
and when a concept's restrictions are defining or not, in other words
both necessary and sufficient as opposed to just necessary. The
aversion to the use of the **all** modifier seems to stem from the
counterintuitive nature of **all** being vacuously true and a
reluctance to make strong assertions. Medicine is full of exceptions. 

Moreover, in building and maintaining large terminologies there are
concerns that have little to do with description logics, all the
normal IT issues such as collaborative workflow, revision history, auditing, and
business process rules and data provenance. Taking a completely description logic centric
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
ids internally. Documents in CouchDB can also have attachments so one can store research papers, image files and so forth with the documents. This make collaborative work smoother, modelers can back up their reasons for modeling a given concept a certain way with attachments that reference the literature.

### Description Logic

The basic terms in a description logic are called **concepts** or **classes** in OWL. Concepts are
defined in terms of other concepts, often called parents, and one can have roles or binary relations between concepts. For example a drug terminology may contain the concepts **anti-infective** and **oral** and have a relation such as **route-of-administration**. The concept **aspirin** may then be defined as:

    (define-concept aspirin (and anti-infective (all route-of-administration oral)))

The key idea is that a concept describes a set of individuals in some universe. Description logics can have quite a lot of features depending on whether they support disjunctions, disjointness, cardinalities and negations. The three main dialects of OWL, Lite, DL, and FULL, support increasing amounts of expressivity in terms of the logic with the tradeoff being the ability to do inferencing. OWL Full for example is largely intractable. 

By restricting the logic to a small set of features, description logics such as Ontylog have been able to support very large vocabularies, classifying them in a reasonable time. The features of Ontylog can be encoded as conjunctions of terms which are essentially binary predicates or triples. In fact OWL itself can be serialized as RDF so that triples are sufficent as storage for description logics.

### Simple Triple Store

Bitstore is taking a bottoms up approach to DL by starting with a simple triple store for each couchdb database. Each record is a triplet of couchdb document ids. So using the example above one might have triples:

    <aspirin, isa, anti-infective>
    <aspirin, route-of-administration oral>

where all terms with the same subject are implicitly conjoined together as part of a concept. The triples contain the ids for the docs, the names above are just for clarity. Notice that **isa** and **route-of-administration** are relations and in general will connect multiples pairs of documents, nevertheless each relation will have a document associated with it, where we might store additional information such as whether the relation is transitive or has a converse. So all the information about what in a DL one might consider the schema, the relations, are stored as docs as well as the concepts but they are still completely independent of one another. The relations betwen docs are stored in the triple store and are orthogonal to the documents. This contrasts with the approach taken by [Riak](http://github.com/zeitgeist/riak/) with its **links** that are contained in the docs. Of course this requires that the triple store stay in sync as the document store is changing.

Initially we used Mnesia for the triple store. It provided a simple solution and inherited all the best of erlang, the QLC stuff was especially nice. Currently we're using [bitcask](http://github.com/basho/bitcask)  which might not seem ideal for storing a graph but early hacks indicate it will be adequate for the small ontologies we're using and it's somewhat simpler to use is for both the triple store and the inverted index. We'll discuss the storage model in more detail if the design proves to be interesting.

### Directed Acyclic Graphs

It's fairly easy to view a triple store as a labelled graph, where each triple <source,arrow,target> represents a labelled arc connecting two nodes in the graph. In descriptions logics like Ontylog, the main inference process is classification, computing the subsumption relation. Ontylog uses what is sometimes called structural classification, it takes the concepts and builds a directed acyclic graph. But one can imagine forming graphs of documents in general, regardless of any logic, so the approach in Bitstore will be to layer the implementation of ontylog on top of a more general graph data structure persisted in a triple store. We think this will add a lot of schema like functionality back into couchdb in a principled way. Users might wish to relate any pair of documents to one another. We will expose the ability to relate docs this way in the REST api. A typical use case might be adding FOAF connections to documents representing people. It's importatn to note that Ontylog clearly distinguishes itself from the current state of the DL world in it's approach to classification. Most, if not all DL reasoners, make use of tableaux techniques which allow for much more expressiveness in the constructs they support. However this comes with a large performance hit. 

### Full Text Indexing

We've also added FTI to Bitstore, embedding the [work](http://github.com/bdionne/bitstore/tree/master/src/search) based on the ideas in the Armstrong book. Currently we index all the values in all the docs and then use the changes api to incrementally update the indices. Originally we stored the indices as couch dbs, which works surprisingly well, but we've been expirementing with different back ends. Currently we are using [bitcask](http://github.com/basho/bitcask) which is an append only key value store. So far the performance of this is excellent. It has a merge operation which accomplishes the same function as couchdb's compact. This is necessary when writing an inverted index as the same records are written often.

So basically:

    bitstore =  couchdb + ontylog + FTI

Or more generally:

    bitstore = couchdb + graph database + FTI + ontylog

One should be able to ignore the DL component ontylog and use it simply as a graph database, or use inference engine other than ontylog

### Building

Bitstore now makes use of rebar. The environment variable at the top of the `Makefile`

    COUCHDB 

can be adjusted according;y. The modified [couchdb](http://github.com/bdionne/couchdb/tree/bitstore) branch now only has some small mods in Futon and the couch.js client libs to extend the APIs. All the erlang code extending
couchdb is now included in bitstore.

   make

will pull the dependency `bitcask`

    make config

will build the `trigrams` and copy the changes needed to configure couchdb into `COUCHDB/etc/couchdb/local_dev.ini'. The changes to the couchdb config file that are needed are [here](http://github.com/bdionne/bitstore/blob/master/config/couch.ini) and can be adjusted as needed.

    make run

will add the need erlang paths and start couchdb using `COUCHDB/utils/run -i' . This provides an interactive shell which can be useful for executing some of the indexer  or classifier commands directly. The -sname option is not required but it useful when using debug tools such as Distel in emacs.


### Opinions

We think this provides an excellent foundation for ontology development. Couchdb's schema-less design coupled with replication and the simplicity of the REST/JSON api will provide an architecturally simple, flexible, and scalable approach to building environments for multiple collaborators to develop ontologies. Having struggled with the enormous impedance mismatch between description logics and relational database, the NoSQL movement could not have come sooner (though I have to say the general tone of the discussion on the net around NoSQL versus SQL has a very bad signal to noise ratio). Reasoning over graphs is just not something relational dbs were built for.

Moreover, trying to use a DL as the center and primary focus of a system has led to many bent screws over the years. CouchDB, together with CouchApps provides a very flexible and robust platform that will enable collaborators to incorporate various workflows and curation processes while keeping them separate from the DL aspects of the terminologies. Adding slots to documents to maintain history or evolution of terms, provenance, etc., requires no change to schemas as there are none. Additionally one can easily support multiple world views of the same set of documents by using multiple triple stores that refer to the same couchdb database. This could help solve the problem of having an epidemiologist and an anatomist both modelling the same terminology with somewhat different ideas as to upper structure and relations of importance.



