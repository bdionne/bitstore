## Prototyping FTI for CouchDB databases in CouchDB

Chapter 20 of Joe Armstrong's <a href="http://www.pragprog.com/titles/jaerlang/programming-erlang">Erlang book</a> provides a nice example of the use of processes to do full text indexing with map/reduce. The essential idea is to spawn a process for each document to index and let the reduce function populate the inverted index as it collects the results of the map phase. I recently heard mention of <a href="http://dukesoferl.blogspot.com/2009/07/osmos.html">osmos</a> in a talk from the NoSQL east conference and it struck me as the ideal data structure for storing an inverted index, particularly since it supports user-defined merging. So when one encounters the word Neoplasm in multiple docs one can just write the key/value to the store and let a defined merging function sort things out.

Being the lazy programmer that I am I downloaded the Erlang code sample and modified it a bit to try it out against CouchDB databases, using osmos for the index store. It worked ok until I tried a somewhat larger corpus of data from <a href="http://bitdiddle.cloudant.com:5984/biomedgt/">cancer genomics</a>. Osmos started crashing, I'm sure the issues were minor but I hadn't read that code so I thought why not just store the index in a couch db for now and come back to osmos later. 

It turns out to work better than you'd think. Each distinct word is a document so it does fill space as more documents are processed and each document is updated more and more, but compaction takes it readily back down to a manageable size.

It runs in the same VM with couchdb, using <a href="http://github.com/jchris/hovercraft">hovercraft</a> to interact with couch and provide the docs in <a href="http://github.com/bdionne/indexer/blob/master/indexer_couchdb_crawler.erl">batches</a> to be analyzed.

## Don't try this at home

But if you do it's not too hard. You need a recent copy of hovercraft in your couchdb install directory. For best results install this project in a sibling directory to couchdb. For convenience I created a [couchdb branch](http://github.com/bdionne/couchdb/tree/bitstore "Bitstore") that will start the indexer along with the couch server. It includes hovercraft in the top directory. So build as you normally would:

    .... make dev

Note: this adds an entry default_dev.ini:

    [httpd_db_handlers]
    ...
    _index = {couch_httpd_db, handle_index_req}


and then compile hovercraft:

    erlc hovercraft.erl

In the indexer directory type:

    make

I typically start couchdb with:

    ERL_FLAGS='-sname couch@localhost -pa ../indexer' ./utils/run -i

assuming hovercraft is compiled and on the path. If indexer is not a sibling directory adjust the -pa accordingly. The indexer can be run directly from the erlang shell:

    indexer:start_link().

The indexer supports multiple dbs. For any db you want indexed type:

    indexer:start("biomedgt").

These last two commands can also be run from the HTTP API, using the [couchdb branch](http://github.com/bdionne/couchdb/tree/lucille "Lucille") mentioned above. The first is started by couch. To index a db:

    curl -X POST http://127.0.0.1:5984/biomedgt/_index

has the same effect as running the indexer:start("biomedgt") command

The first time a db is idexed it creates a new database, .eg. biomedgt-idx to store the index. It also stores checkpoint information in the index db for help in the event of restart. The db can be searched even while it's being indexed:

    indexer:search("biomedgt","Rat Man").
    

And with luck you see these <a href="http://gist.github.com/247784">messages</a>.

It takes a checkpoint after indexing every n docs, so you can call:

    indexer:stop("biomedgt") 

and then:

    indexer:start("biomedgt") 

again and it resumes. Note that it actually first schedules the stop so that any existing operations can finish. What gets indexed is all the values in the docs but not the keys or _xxx fields. After the db is indexed it start polling for changes every 60 seconds. If new documents are inserted or changed it indexes those and updates the indices appropriately. *Note: deletes are not yet supported, as I can't yet figure out how to retrieve a deleted document.*

## Still quite buggy

Since everything needed to support indexing is in a couchdb db, one can just delete the "db_name-idx" database to start over

## Motivation and Ideas

I think Lucene is pretty much state of the art these days for Java-based text indexing but I've been thinking it'd be nice to have something more native to CouchDB and have been curious as to how well Erlang can handle this.

Currently we index all the slot values, skipping the reserved _xxx slots and the slot names. CouchDB is schema-less but presumably in most dbs docs would be fairly homogenous in having the same slot names across multiple docs. We also don't require the user to declare viewsthat are used to construct what gets indexed. This is the normal approach with Lucene style indexing. We just index everything and think it might be useful to allow filters to be defined that run over the search results. Next steps will be to also record the slot names as well as doc ids in the inverted index so we can be more specific about where a term was found.

This is just a prototype, to explore the issues with FTI in erlang running in the couchdb VM. Clearly storing an inverted index in a couchdb db is sort of lame, but for a moderate size db like biomedgt, about 65K docs in 112M, it works surprisingly well. The initial index db is quite large due to repeated writes to the same docs but this is easily controlled with compaction. The original size is about 3.5 gig but it reduces to 90M or so. This prototype is also good for using hovercraft and exploring issues with writing code at the level of the storage engine. The indexing could really be done at even a lower level but this would involve changes to core components that need to be refactored first. 

## Next TODOs

* add some APIs at the HTTP level

As noted above the indexer can be started for a database using:

    curl -X POST http://127.0.0.1:5984/biomedgt/_index

Once it's started, simple queries can be run against it:

    curl http://127.0.0.1:5984/biomedgt/_index_query?word=neoplasm%20rat%20benign

To find all documents that contain the words neoplasm, rat, and benign. Of course the results aren't completely available until the indexing is complete. Support has been added to check the task status in Futon.


* add slot names to the index

* add result filtering

* revisit using osmos instead of couchdb for index persistence






 
