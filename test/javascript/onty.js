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

// Do some basic test of bitstore
couchTests.onty = function(debug) {
  var result = JSON.parse(CouchDB.request("GET", "/").responseText);
  T(result.couchdb == "Welcome");

  var db = new CouchDB("test_onty", {"X-Couch-Full-Commit":"true"});
    db.deleteDb();
    db.createDb();
    var pred = {name : "isa", type : "role"};
    var result = db.save(pred);

    T(result.ok==true); 
    T(result.id);
    T(result.rev); 

    pred = result;

    var subj = {name : "aspirin", type : "concept"};
    result = db.save(subj);

    T(result.ok==true); 
    T(result.id);
    T(result.rev); 
    
    subj = result;

    var obj = {name : "drug", type : "concept"};
    result = db.save(obj);

    T(result.ok==true); 
    T(result.id);
    T(result.rev);

    obj = result;

    T(db.addRelation(subj,pred,obj).ok);

    result = db.getRelationValues(subj,pred);
    T(db.last_req.status == 200);
    T(result.total_rows == result.rows.length);
    T(result.rows.length == 1);
    print(result.rows[0].type);

   result = db.getDefinition(subj);
   T(db.last_req.status == 200);
   T(result.total_rows == result.rows.length);
   T(result.rows.length == 1);
    print(result.rows[0].pred.name);
    print(result.rows[0].vals[0].name);

    

};
