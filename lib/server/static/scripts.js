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

function loop() {
    myGet('./dump/graph.dot', function(data) {
        inputGraph = data.response;
        console.log("graph loaded");
        draw();
//        setTimeout(loop, 1000);
    });
};

function draw() {
    var result = graphlibDot.parse(inputGraph);
    var renderer = new dagreD3.Renderer();

    // Custom transition function
    function transition(selection) {
        return selection.transition().duration(500);
    }

    renderer.transition(transition);
    var layout = renderer.run(result, d3.select("svg g"));
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
