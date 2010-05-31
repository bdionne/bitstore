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

By restricting the logic to a small set of features, description logics such as Ontylog have been able to support very large vocabularies, classifying them in reasonable time. The features of Ontylog can be encoded as conjunctions of terms which are essentially binary predicates or triples. In fact OWL itself can be serialized as RDF so that triples are sufficent as storage for description logics.

### Simple Triple Store

Bitstore is taking a bottoms up approach to DL by starting with a simple triple store for each couchdb database. Each record is a triplet of couchdb document ids. So using the example above one might have triples:

    <aspirin, isa, anti-infective>
    <aspirin, route-of-administration oral>

where all terms with the same subject are implicitly conjoined together as part of a concept. The triples contain the ids for the docs, the names above are just for clarity. Notice that **isa** and **route-of-administration** are relations and in general will connect multiples pairs of documents, nevertheless each relation will have a document associated with it, where we might store additional information such as whether the relation is transitive or has a converse. So all the information about what in a DL one might consider the schema, the relations, are stored as docs as well as the concepts but they are still completely independent of one another. The relations betwen docs are stored in the triple store and are orthogonal to the documents. This contrasts with the approach taken by [Riak](http://github.com/zeitgeist/riak/) with its **links** that are contained in the docs. Of course this requires that the triple store stay in sync as the document store is changing.

Initially we're using Mnesia for the triple store. It provides a simple solution and inherits all the best of erlang, nodes can be replicated, etc.. It does have a size limitation but we're storing triples of integers so it will be a while before we run into any limits. This piece will also be readily replaceable, .eg. if at some point couchdb enables a tighter integration in the same vm. 

### Directed Acyclic Graphs

It's fairly easy to view a triple store as a labelled graph, where each triple <source,arrow,target> represents a labelled arc connecting two nodes in the graph. In descriptions logics like Ontylog, the main inference process is classification, computing the subsumption relation. Ontylog uses what is sometimes called structural classification, it takes the concepts and builds a directed acyclic graph. But one can imagine forming graphs of documents in general, regardless of any logic, so the approach in Bitstore will be to layer the implementatin of ontylog on top of a more general graph data structure persisted in a triple store. We think this will add a lot of schema like functionality back into couchdb in a principled way. Users might wish to relate any pair of documents to one another. We will expose the ability to relate docs this way in the REST api. A typical use case might be adding FOAF connections to documents representing people. 

### Full Text Indexing

We've also added FTI to Bitstore, embedding the [work](http://github.com/bdionne/bitstore/tree/bitcask/src/search) based on the ideas in the Armstrong book. Currently we index all the values in all the docs and then use the changes api to incrementally update the indices. Originally we stored the indices as couch dbs, which works surprisingly well, but we've been expirementing with different back ends. Currently we are using [bitcask](http://github.com/basho/bitcask) which is an append only key value store. So far the performance of this is excellent. It has a merge opertaion which accomplishes the same function as couchdb's compact. This is necessary when writing an inverted index as the same records are written often.

So basically:

    bitstore =  couchdb + ontylog + FTI

Or more generally:

    bitstore = couchdb + graph database + FTI + ontylog

One should be able to ignore the DL component ontylog and use it simply as a graph database, or use inference engine other than ontylog

### Building

As mentioned above, in order to build and run a slightly modified version of [couchdb](http://github.com/bdionne/couchdb/tree/bitstore) is needed. For best results bitstore and couchdb should be installed at the same level in the file system, .eg.

    ~/emacs/couchdb
    ~/emacs/bitstore

Bitcask is also needed:

    ~/emacs/bitcask

Bitcask is easy to build, just type make in the top level. You can modify the rebar.config to use git for a dependency bitcask needs (ebloom) if you don't have hg installed

The changes to the couchdb config file that are needed are [here](http://github.com/bdionne/bitstore/blob/config/couch.ini) and can be added to local_dev.ini in couchdb/etc/couchdb. Bitstore now has a single top level make that builds both the indexer and ontylog, compiling the erlang into the ebin directory. The -I include option in the Makefile needs to specify the location of couchdb's header. 

A copy of ~/emacs/bitstore/src/couchdb/couch_httpd\_bitstore.erl should be placed in ~/emacs/couchch/src/couchdb and an entry added to ~/emacs/couchdb/src/couchdb/Makefile.am. This has already been done if you checkout the couchdb branch mentioned above.

The hardest part is starting couch correctly with all the needed paths. 

    cd ~/emacs/couchdb

    ERL_FLAGS="-sname couch@localhost -pa ../bitstore/ebin -pa ../bitcask/ebin -pa ../bitcask/deps/ebloom/ebin ./utils/run -i

Notice the command use relative paths to pick up files from the bitstore project. 


### Opinion

We think this provides an excellent foundation for ontology development. Couchdb's schema-less design coupled with replication and the simplicity of the REST/JSON api will provide an architecturally simple, flexible, and scalable approach to building environments for multiple collaborators to develop ontologies. Having struggled with the enormous impedance mismatch between description logics and relational database, the NoSQL movement could not have come sooner. Reasoning over graphs is just not something relational dbs were built for.

Moreover, trying to use a DL as the center and primary focus of a system has led to many bent screws over the years. CouchDB, together with CouchApps provides a very flexible and robust platform that will enable collaborators to incorporate various workflows and curation processes while keeping them separate from the DL aspects of the terminologies. Adding slots to documents to maintain history or evolution of terms, provenance, etc., requires no change to schemas as there are none. Additionally one can easily support multiple world views of the same set of documents by using multiple triple stores that refer to the same couchdb database. This could help solve the problem of having an epidemiologist and an anatomist both modelling the same terminology with somewhat different ideas as to upper structure and relations of importance.



