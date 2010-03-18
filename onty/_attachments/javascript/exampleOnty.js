var Log = {
    elem: false,
    write: function(text){
        if (!this.elem) 
            this.elem = document.getElementById('log');
        this.elem.innerHTML = text;
        this.elem.style.left = (500 - this.elem.offsetWidth / 2) + 'px';
    }
};

function addEvent(obj, type, fn) {
    if (obj.addEventListener) obj.addEventListener(type, fn, false);
    else obj.attachEvent('on' + type, fn);
};


function init(){
  var db = new CouchDB("biomedgtnew", {"X-Couch-Full-Commit":"true"});
  //var isa_pred = db.open("6776ae29980ce9b2b243a4683b032dff");
  //var isa_pred = db.open("12e3051b908224686fcf7ea3bd031c43");
  //var isa_pred = db.open("6776ae29980ce9b2b243a4683b032dff");
  //var isa_pred = db.open("0e370708005d8bb206046e406e002ed7");
  var isa_pred = db.open("6776ae29980ce9b2b243a4683b032dff");
  //var isa_pred = db.open("48daa7d79ba5a269c76d25d6d300079a");

  


  //alert(isa_pred._id);

  var result = db.getRoots(isa_pred);

  // alert("there are " + result.rows.length + " roots");

  var roots = [];

  for (var i = 0; i < result.rows.length; i++) {

    roots[i] = "{id:\"" + result.rows[i]._id +
               "\", name:\"" + result.rows[i].code +
      "\", data:{}, children:[]}";
    

  }

  
  
  var json = "{id:\"root\", name:\"Thing\", data:{}, children:[" + roots + "]}";

  //alert(json);
    
    //A client-side tree generator
    var getTree = (function() {
        var i = 0;
        return function(nodeId, level) {
          // alert("calling for " + nodeId + "  " + level);
          result = db.getInvRelationValues(isa_pred,{"_id": nodeId});
          var sub_nodes = [];
          for (var i = 0; i < result.rows.length; i++) {
            sub_nodes[i] = eval('(' + "{id:\"" + result.rows[i]._id +
               "\", name:\"" + result.rows[i].code +
                                "\", data:{}, children:[]}" + ')');
            //alert(sub_nodes[i]);
          }

          //alert(sub_nodes + " of length " + sub_nodes.length);

          //alert(sub_nodes[0].id);
          
          return {
              'id': nodeId,
                'children': sub_nodes
          };
        };
    })();
    
    var infovis = document.getElementById('infovis');
    var w = infovis.offsetWidth, h = infovis.offsetHeight;
    //init canvas
    //Create a new canvas instance.
    var canvas = new Canvas('mycanvas', {
        'injectInto': 'infovis',
        'width': w,
        'height': h,
        'backgroundColor': '#1a1a1a'
    });
    //end
    
    
    //Implement a node rendering function called 'nodeline' that plots a straight line
    //when contracting or expanding a subtree.
    ST.Plot.NodeTypes.implement({
        'nodeline': function(node, canvas, animating) {
            if(animating === 'expand' || animating === 'contract') {
                var pos = node.pos.getc(true), nconfig = this.node, data = node.data;
                var width  = nconfig.width, height = nconfig.height;
                var algnPos = this.getAlignedPos(pos, width, height);
                var ctx = canvas.getCtx(), ort = this.config.orientation;
                ctx.beginPath();
                if(ort == 'left' || ort == 'right') {
                    ctx.moveTo(algnPos.x, algnPos.y + height / 2);
                    ctx.lineTo(algnPos.x + width, algnPos.y + height / 2);
                } else {
                    ctx.moveTo(algnPos.x + width / 2, algnPos.y);
                    ctx.lineTo(algnPos.x + width / 2, algnPos.y + height);
                }
                ctx.stroke();
            } 
        }
    });

    //init st
    //Create a new ST instance
    var st = new ST(canvas, {
        //set duration for the animation
        duration: 800,
        //set animation transition type
        transition: Trans.Quart.easeInOut,
        //set distance between node and its children
        levelDistance: 15,
        //set max levels to show. Useful when used with
        //the request method for requesting trees of specific depth
        levelsToShow: 15,
        //set node and edge styles
        //set overridable=true for styling individual
        //nodes or edges
        Node: {
            height: 20,
            width: 60,
            //use a custom
            //node rendering function
            type: 'nodeline',
            color:'#23A4FF',
            lineWidth: 2,
            align:"center",
            overridable: true
        },
        
        Edge: {
            type: 'bezier',
            lineWidth: 2,
            color:'#23A4FF',
            overridable: true
        },
        
        //Add a request method for requesting on-demand json trees. 
        //This method gets called when a node
        //is clicked and its subtree has a smaller depth
        //than the one specified by the levelsToShow parameter.
        //In that case a subtree is requested and is added to the dataset.
        //This method is asynchronous, so you can make an Ajax request for that
        //subtree and then handle it to the onComplete callback.
        //Here we just use a client-side tree generator (the getTree function).
        request: function(nodeId, level, onComplete) {
          //alert("calling for subs");
          var ans = getTree(nodeId, level);
          //alert(ans.name + " the kids " + ans.children);
          onComplete.onComplete(nodeId, ans);  
        },
        
        onBeforeCompute: function(node){
            Log.write("loading " + node.name);
        },
        
        onAfterCompute: function(){
            Log.write("done");
        },
        
        //This method is called on DOM label creation.
        //Use this method to add event handlers and styles to
        //your node.
        onCreateLabel: function(label, node){
            label.id = node.id;            
            label.innerHTML = node.name;
            label.onclick = function(){
                st.onClick(node.id);
            };
            //set label styles
            var style = label.style;
            style.width = 40 + 'px';
            style.height = 17 + 'px';            
            style.cursor = 'pointer';
            style.color = '#fff';
            //style.backgroundColor = '#1a1a1a';
            style.fontSize = '0.8em';
            style.textAlign= 'center';
            style.textDecoration = 'underline';
            style.paddingTop = '3px';
        },
        
        //This method is called right before plotting
        //a node. It's useful for changing an individual node
        //style properties before plotting it.
        //The data properties prefixed with a dollar
        //sign will override the global node style properties.
        onBeforePlotNode: function(node){
            //add some color to the nodes in the path between the
            //root node and the selected node.
            if (node.selected) {
                node.data.$color = "#ff7";
            }
            else {
                delete node.data.$color;
            }
        },
        
        //This method is called right before plotting
        //an edge. It's useful for changing an individual edge
        //style properties before plotting it.
        //Edge data proprties prefixed with a dollar sign will
        //override the Edge global style properties.
        onBeforePlotLine: function(adj){
            if (adj.nodeFrom.selected && adj.nodeTo.selected) {
                adj.data.$color = "#eed";
                adj.data.$lineWidth = 3;
            }
            else {
                delete adj.data.$color;
                delete adj.data.$lineWidth;
            }
        }
    });
    //load json data
    st.loadJSON(eval( '(' + json + ')' ));
    //compute node positions and layout
    st.compute();
    //emulate a click on the root node.
    st.onClick(st.root);
    //end
    //Add event handlers to switch spacetree orientation.
   function get(id) {
      return document.getElementById(id);  
    };

    var top = get('r-top'), 
    left = get('r-left'), 
    bottom = get('r-bottom'), 
    right = get('r-right');
    
    function changeHandler() {
        if(this.checked) {
            top.disabled = bottom.disabled = right.disabled = left.disabled = true;
            st.switchPosition(this.value, "animate", {
                onComplete: function(){
                    top.disabled = bottom.disabled = right.disabled = left.disabled = false;
                }
            });
        }
    };
    
    top.onchange = left.onchange = bottom.onchange = right.onchange = changeHandler;
    //end

}
