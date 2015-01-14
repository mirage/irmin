function getXmlDoc() {
  var xmlDoc;

  if (window.XMLHttpRequest) {
    // code for IE7+, Firefox, Chrome, Opera, Safari
    xmlDoc = new XMLHttpRequest();
  }
  else {
    // code for IE6, IE5
    xmlDoc = new ActiveXObject("Microsoft.XMLHTTP");
  }

  return xmlDoc;
}

function myGet(url, callback) {
  var xmlDoc = getXmlDoc();

  xmlDoc.open('GET', url, true);

  xmlDoc.onreadystatechange = function() {
    if (xmlDoc.readyState === 4 && xmlDoc.status === 200) {
      callback(xmlDoc);
    }
  }

  xmlDoc.send();
}

var inputGraph;
var oldGraph = false;
var depth = 4;
var full = 0;
var reload = 1;
var graph_dot;
var old_graph_dot;

function read_query() {
    var depthRE  = /[?&]depth=([^&]+)/;
    var depthMatch = window.location.search.match(depthRE);
    if (depthMatch) {
	depth = decodeURIComponent(depthMatch[1]);
    }
    var fullRE  = /[?&]full=([^&]+)/;
    var fullMatch = window.location.search.match(fullRE);
    if (fullMatch) {
	full = decodeURIComponent(fullMatch[1]);
    }
    var reloadRE = /[?&]reload=([^&]+)/;
    var reloadMatch = window.location.search.match(reloadRE);
    if (reloadMatch) {
	reload = decodeURIComponent(reloadMatch[1]);
    }
    console.log("query [depth="+depth+" reload="+reload+" full="+full+"]");
}

function loop() {
    read_query();
    myGet('./graph/graph.dot?depth='+depth+'&full='+full, function(data) {
        graph_dot = data.response;
	if (old_graph_dot != graph_dot) {
	    inputGraph = graphlibDot.parse(graph_dot);
	    console.log("graph parsed!");
	    inputGraph.eachNode(function(u, value) {
		if (oldGraph && !oldGraph.hasNode(u)) {
		    value.label = "<div class='updated'>"+value.label+"</div>";
		};
	    });
            draw();
	    oldGraph = inputGraph;
	};
	old_graph_dot = graph_dot;
	if (reload != 0) {
            setTimeout(loop, reload * 1000);
	};
    });
};

function draw() {
    var renderer = new dagreD3.Renderer();
    // Custom transition function
    function transition(selection) {
	return selection.transition().duration(500);
    }
    renderer.transition(transition);
    var layout = renderer.run(inputGraph, d3.select("svg g"));
    transition(d3.select("svg"))
        .attr("width", layout.graph().width + 40)
        .attr("height", layout.graph().height + 40);
    d3.select("svg")
        .call(d3.behavior.zoom().on("zoom", function() {
            var ev = d3.event;
            svg.select("g")
		.attr("transform", "translate(" + ev.translate + ") scale(" + ev.scale + ")");
        }));
}
