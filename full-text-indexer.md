## FTI for CouchDB databases

Chapter 20 of Joe Armstrong's <a href="http://www.pragprog.com/titles/jaerlang/programming-erlang">Erlang book</a> provides a nice example of the use of processes to do full text indexing with map/reduce. The essential idea is to spawn a process for each document to index and let the reduce function populate the inverted index as it collects the results of the map phase.

This is the third version of this prototype and it incorporates one new feature in addition to a number of small features, packaging changes, etc.. that make it play nicer with CouchDB.

The first version of this used a couch db to store the inverted index. Since the second verion we've been using [Bitcask](http://www.basho.com/developers.html#bitcask), which is really rock solid. So much so that I forget it's there. I suppose to be even lighter we could use couchdb's storage model similar to the way [GeoCouch](http://vmx.cx/couchdb/tutorial/indexer.html) is integrated, but I'm not sure how adequate that model is for an inverted index. In the first version that used a couch db to store the index, a compaction step was needed after indexing a db due to the multiple writes to the same doc. Bitcask also requires a compaction step (they call it merging as they actually use multiple files similar to BDB). I'll leave that for later when I look at scaling.

 A gen\_server is started in couch as a daemon which will start a different gen\_server for each database to be indexed. A [crawler](http://github.com/bdionne/bitstore/blob/master/src/search/indexer_couchdb_crawler.erl) uses the b-tree to run over the database collecting 1K docs at a time to index. It checkpoints periodically to help with restart should something go wrong. After it's completed it periodically polls for new docs, using the changes functions to incrementally update the indices. How often it polls is now configurable as is whether to automatically index new dbs as they are created. These settings are in the couchdb section of the local.ini file:

    [couchdb]
    ;; automitcally index new dbs
    fti_dbs = true
    ;; how often to poll for new updates (in milliseconds)
    fti_poll_interval = 10000

The indices are now stored in the same location as the databases, in a directory called fti. Each has it's own directory whose name is the database name with an "-idx" suffix.

Assuming this is built and run as specified in the main [Readme.md](http://github.com/bdionne/bitstore), the indexer can be started on a database from the command line, .e.g.:

    indexer:start_indexing("biomedgt").

or via the REST API:

    curl -X POST http://127.0.0.1:5984/biomedgt/_index

It will start automatically if configured as above. The db can be searched even while it's being indexed:

    indexer:search("biomedgt","benign neoplasm scrotum").

Once it's completely indexed a database, it periodically polls for changes and updates as needed. While running the current status is observable in Futon in the status panel. If a database is deleted the indices are also deleted.

What's new in this version is the ability to filter searches by field names in the documents. As the database is indexed the field names encountered are tracked and stored. These can be retrieved using:

    curl -X POST http://127.0.0.1:5984/biomedgt/_index_slots

If you're running the modified [couchdb branch](http://github.com/bdionne/couchdb/tree/bitstore) notice in Futon that the jump to: text field has been replaced by Search for: and next to it is a new field Filter by: which is a drop down populated by the field names discovered by the indexer. Select one and the search field to the left will now be filtered by that slot name. 

The indexer can also be stopped and restarted:

    indexer:stop_indexing("biomedgt") 

and then:

    indexer:start_indexing("biomedgt") 

Queries can also be run with the REST API:

    curl 'http://127.0.0.1:5984/biomedgt/_index_query?word=neoplasm heart benign'

Or to filter:

    curl 'http://127.0.0.1:5984/biomedgtnew/_index_query?word=malignant&field=Preferred_Name'



## A little less buggy

This version added a lot of little features, that relate to it's use and not so much the core functionality. How it fits with the rest of Bitstore will determine how these evolve. It's changed quite a bit as I learn the ins and outs of gen_servers. For small dbs it's pretty useful, particularly with the Futon hacks. You can now find stuff!! Oh, also the default field name in the Filter By drop down is ID, so you can still search over IDs as Futon currently works today. This is useful if you create your own IDs rather than using integers.

## Motivation and Ideas

I think Lucene is pretty much state of the art these days for Java-based text indexing but I've been thinking it'd be nice to have something more native to CouchDB and have been curious as to how well Erlang can handle this. I'm also interested in semantic search and eventually plan to integrate search with ontylog, so that "myocardial infarction" can be found when searching "heart attack".

CouchDB is schema-less but presumably in most dbs docs would be fairly homogenous in having the same slot names across multiple docs. Lucene requires one use a design doc to specify what to index, similar to how one builds views. This works well but requires soem knowledge of the scheme up front. Of course one could just index the entire doc using Lucene, which is essentially what this indexer does. It then also tracks the slots encountered to filter searches after the fact. If a database was truly schema-less, with every doc having a distinct set of slots, then one could readily also build an index over the slot names and search those as well.


## Next TODOs

The next major task is to integrate this indexer with the rest of Bitstore, using sematic navigation over the realtionships between the concepts to better inform the search. The search itself needs a lot more enhancement, support for wildcards and prioritization of results, better handling of complex chemical names, etc..











 
