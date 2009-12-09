## Prototyping FTI for CouchDB databases in Osmos

This branch of the [indexer](http://github.com/bdionne/indexer) is the second attempt at prototyping FTI in CouchDB, this time we're using [osmos](http://dukesoferl.blogspot.com/2009/07/osmos.html) for persistence of the inverted index. Osmos was designed for storage that might have heavy write load. One of the key features is support for user defined mergers. When indexing a large corpus every toime the same normalized word is found another entry for that key is stored in the index. Being able to define how the values are merged seems like it may be quite useful.

We start with more or less same approach as on the [master branch](http://github.com/bdionne/indexer) except we are not tracking which slots a word is found in during indexing, only that it was found in a given doc. 



## Don't try this at home

But if you do it's not too hard. You need a recent copy of hovercraft in your couchdb install directory. For best results install this project in a sibling directory to couchdb. For convenience I created a [couchdb branch](http://github.com/bdionne/couchdb/tree/lucille "Lucille") that will start the indexer along with the couch server. It includes hovercraft in the top directory. So build as you normally would:

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






 
