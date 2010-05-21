// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

// Do some basic tets of indexing and searching
couchTests.index = function(debug) {
  var result = JSON.parse(CouchDB.request("GET", "/").responseText);
  T(result.couchdb == "Welcome");

  var db = new CouchDB("test_search", {"X-Couch-Full-Commit":"true"});
  db.deleteDb();
  db.createDb();
  var doc = {foo : "foo"};
  var result = db.save(doc);

  
  T(result.ok==true); // return object has an ok member with a value true
  T(result.id); // the _id of the document is set.
  T(result.rev); // the revision id of the document is set.

  T(db.index().ok);
  T(db.last_req.status == 202);
  
  result = db.searchDocs({word : "foo"});
  T(db.last_req.status == 200);
  T(result.total_rows == result.rows.length);

  T(db.index({stop:"true"}).ok);
  T(db.last_req.status == 202);
  
			 

};
