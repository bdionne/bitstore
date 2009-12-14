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

// Used by replication test
if (typeof window == 'undefined' || !window) {
  CouchDB.host = "127.0.0.1:5984";
  CouchDB.inBrowser = false;
} else {
  CouchDB.host = window.location.host;
  CouchDB.inBrowser = true;
}

var couchTests = {};

function loadTest(file) {
  loadScript("../"+file);
};


// keep sorted
loadTest("index.js");


function makeDocs(start, end, templateDoc) {
  var templateDocSrc = templateDoc ? JSON.stringify(templateDoc) : "{}"
  if (end === undefined) {
    end = start;
    start = 0;
  }
  var docs = []
  for (var i = start; i < end; i++) {
    var newDoc = eval("(" + templateDocSrc + ")");
    newDoc._id = (i).toString();
    newDoc.integer = i;
    newDoc.string = (i).toString();
    docs.push(newDoc)
  }
  return docs;
}

function run_on_modified_server(settings, fun) {
  try {
    // set the settings
    for(var i=0; i < settings.length; i++) {
      var s = settings[i];
      var xhr = CouchDB.request("PUT", "/_config/" + s.section + "/" + s.key, {
        body: JSON.stringify(s.value),
        headers: {"X-Couch-Persist": "false"}
      });
      CouchDB.maybeThrowError(xhr);
      s.oldValue = xhr.responseText;
    }
    // run the thing
    fun();
  } finally {
    // unset the settings
    for(var j=0; j < i; j++) {
      var s = settings[j];
      if(s.oldValue == "\"\"\n") { // unset value
        CouchDB.request("DELETE", "/_config/" + s.section + "/" + s.key, {
          headers: {"X-Couch-Persist": "false"}
        });
      } else {
        CouchDB.request("PUT", "/_config/" + s.section + "/" + s.key, {
          body: s.oldValue,
          headers: {"X-Couch-Persist": "false"}
        });
      }
    }
  }
}

function stringFun(fun) {
  var string = fun.toSource ? fun.toSource() : "(" + fun.toString() + ")";
  return string;
}

function restartServer() {
  CouchDB.request("POST", "/_restart");
}
