## Prototyping FTI for CouchDB databases in CouchDB

Chapter 20 of Joe Armstrong's <a href="http://www.pragprog.com/titles/jaerlang/programming-erlang">Erlang book</a> provides a nice example of the use of processes to do full text indexing with map/reduce. The essential idea is to spawn a process for each document to index and let the reduce function populate the inverted index as it collects the results of the map phase. 

The first version of this used a couch db to stroe the inverted index, this verison uses bitcask. A gen_server is started in couch as a daemon whic will start a different gen_server for each database ot be indexed. It uses a modified version of <a href="http://github.com/jchris/hovercraft">hovercraft</a> to interact with couch and provide the docs in <a href="http://github.com/bdionne/bitstore/blob/bitcast/src/search/indexer_couchdb_crawler.erl">batches</a> to be analyzed. The inverted index is stored in a bitcask which has the same same as the db with a "-idx" suffix.

Assuming this is built and run as specified in the main [Readme.md](http://github.com/bdionne/bitstore/tree/bitcask), the indexer can be started on a database from the command line, .e.g.:

    indexer:start("biomedgt").

or via the REST API:

    curl -X POST http://127.0.0.1:5984/biomedgt/_index

The first time a db is idexed it creates a new bitcask, .eg. biomedgt-idx to store the index. It also stores checkpoint information in the bitcask for help in the event of restart. The db can be searched even while it's being indexed:

    indexer:search("biomedgt","benign neoplasm scrotum").

Once it's completed indexed a databse, it periodically polls for changes and updates as needed. While running the current status is observable in Futon in the status panel. If you're running the modified couchdb branch notice in Futon that the jump to: text field has been replaced by search: and one can type text to search for documents in a database that has been indexed.

The indexer can also be stopped:

    indexer:stop("biomedgt") 

and then:

    indexer:start("biomedgt") 

again and it resumes. Note that it actually first schedules the stop so that any existing operations can finish. What gets indexed is all the values in the docs but not the keys or _xxx fields. After the db is indexed it start polling for changes every 60 seconds. If new documents are inserted or changed it indexes those and updates the indices appropriately. *Note: deletes are not yet supported, as I can't yet figure out how to retrieve a deleted document.*

## Still quite buggy

Since everything needed to support indexing is in a couchdb db, one can just delete the "db_name-idx" database to start over

## Motivation and Ideas

I think Lucene is pretty much state of the art these days for Java-based text indexing but I've been thinking it'd be nice to have something more native to CouchDB and have been curious as to how well Erlang can handle this. I'm also interested in semantic search and eventually plan to integrate search with ontylog, so that "myocardial infarction" can be found when searching "heart attack"

Currently we index all the slot values, skipping the reserved _xxx slots and the slot names. CouchDB is schema-less but presumably in most dbs docs would be fairly homogenous in having the same slot names across multiple docs. We also don't require the user to declare viewsthat are used to construct what gets indexed. This is the normal approach with Lucene style indexing. We just index everything and think it might be useful to allow filters to be defined that run over the search results.

We now track which fields in the docs contain the search strings as well so that clients can support text highligting. This also allows the results to be filtered. This will likely be best supported via integration with the query_server similar to the shows capability.

This is still largely a prototype, to explore the issues with FTI in erlang running in the couchdb VM.
With the new bitcask back end we now indeinx the 65K concepts of biomedgt in under a minute. This is roughly 300M of text. What's even more impressive is the search speed. Incrmental type ahead search is good. 

Queries can also be run with the REST API:

    curl 'http://127.0.0.1:5984/biomedgt/_index_query?word=neoplasm heart benign'

## Next TODOs

* add some APIs at the HTTP level

As noted above the indexer can be started for a database using:

    curl -X POST http://127.0.0.1:5984/biomedgt/_index








 
