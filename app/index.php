<?php

  if(count($_GET) > 0) {
    if(!empty($_GET['legislature'])) $page = basename($_GET['legislature']);
    if(!empty($_GET['chamber'])) $ch = basename($_GET['chamber']);
  }

  if(!isset($page)) $page = "14";
  if($page == '') $page = "14";

  if(!isset($ch)) $ch = "se";
  if($ch == '') $ch = "se";
  
  if($ch == "an")
    $chamber = "Assemblée nationale";
  else
    $chamber = "Sénat";
  
  $array = array(
    "14" => "2012&mdash;2017",
    "13" => "2007&mdash;2012",
    "12" => "2002&mdash;2007",
    "11" => "1997&mdash;2002",
    "10" => "1993&mdash;1997",
    "9" => "1988&mdash;1993",
    "8" => "1986&mdash;1988");
  $class = array(
    "14" => "",
    "13" => "",
    "12" => "",
    "11" => "",
    "10" => "",
    "9" => "",
    "8" => "");
  $class[$page] = "here";

  // ongoing legislature
  $be = "was";
  if($page == '14') $be = "is";

  $have = "had";
  if($page == '14') $have = "has had";
  
  // initial caption
  if($ch == "an")
    $caption = '<p>This graph shows French Members of Parliament (MPs) during the ' . $page . 'th&nbsp;legislature. A link between two MPs indicates that they have cosponsored at least one amendment, bill or resolution.';
  else
    $caption = '<p>This graph shows French Senators during the ' . $page . 'th&nbsp;legislature. A link between two Senators indicates that they have cosponsored at least one amendment, bill or resolution.';

  $caption = $caption . ' Their size is proportional to their <a href="http://toreopsahl.com/tnet/weighted-networks/node-centrality/">weighted degree</a>.</p>'
?>

<!doctype html>
<html>
<head>
  <title><?php
    echo "Cosponsorship networks in the French Parliament: ";
    if($ch == "an")
      echo "Assemblée nationale, législature ";
    else
      echo "Sénat, législature ";
    echo $page;
    ?>
  </title>
  <meta charset="utf-8">
  <link href="http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,600" rel="stylesheet" type="text/css" />
  <link href="assets/styles.css" rel="stylesheet" type="text/css" />
  <link rel="stylesheet" href="font-awesome-4.0.3/css/font-awesome.min.css">
  <style type="text/css" media="screen">
  html, body {
    font: 24px/150% "Source Sans Pro", sans-serif;
    background-image: url("assets/hemicycle_<?php echo $ch; ?>.jpg");
    color: #fff;
    margin: 0;
    padding:0;
    width: 100%;
    height: 100%;
  }
  </style>
  <!--[if lt IE 8]>
    <link rel="stylesheet" href="assets/stylesheets/social_foundicons_ie7.css">
  <![endif]-->
</head>
<body>

<div id="sigma-container">
  <div id="controls" class="bg_<?php echo $ch; ?>">
    <h1>cosponsorship networks</h1>    
    <h2><a href="<?php if($ch=="an") echo "http://assemblee-nationale.fr/"; else echo "http://senat.fr/"; ?>" title="<?php echo $chamber; ?>">
           <img src="assets/logo_<?php echo $ch; ?>.png" height="25" alt="logo">
        </a>&nbsp;<?php echo $chamber . ", " . $array[ $page ]; if($ch=="se") echo "<br>&nbsp;"; ?></h2>

    <p>Chamber&nbsp;&nbsp;
      <?php

      if($ch == "an")
        echo "<a href='?chamber=an&amp;legislature=$page' class='here'>National Assembly</a>";
      else
        echo "<a href='?chamber=an&amp;legislature=$page'>National Assembly</a>";
      echo "&nbsp;&nbsp;";

      if($ch == "se")
        echo "<a href='?chamber=se&amp;legislature=$page' class='here'>Senate</a>";
      else
        echo "<a href='?chamber=se&amp;legislature=$page'>Senate</a>";
      ?>
      <br>
      Legislature&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=8" class='<?php echo $class["8"]; ?>'>1986&mdash;1988</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=9" class='<?php echo $class["9"]; ?>'>1988&mdash;1993</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=10" class='<?php echo $class["10"]; ?>'>1993&mdash;1997</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=11" class='<?php echo $class["11"]; ?>'>1997&mdash;2002</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=12" class='<?php echo $class["12"]; ?>'>2002&mdash;2007</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=13" class='<?php echo $class["13"]; ?>'>2007&mdash;2012</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=14" class='<?php echo $class["14"]; ?>'>2012&mdash;2017</a>
      <label title="Map MPs to their constituencies (very approximative).">
        &nbsp;<input type="checkbox" id="showMap" />
        Map
      </label>
    </p>
    <p>
      Click a node to show its ego network. Double click to zoom in or out.<br>
      Hide&nbsp;
      <label title="Do not draw network ties (vertex edges).">
        <input type="checkbox" id="showEdges" />
        Edges
      </label>
      &nbsp;
      <label title="Do not add labels to nodes (MP names) when zooming in.">
        <input type="checkbox" id="showLabels" />
        Labels
      </label>
      &nbsp;
      <label title="Draw only ties formed among frequent cosponsors (edge weight > 0.5).">
        <input type="checkbox" id="showSparse" />
        Weak ties
      </label>
      <br>
      Download&nbsp;&nbsp;<i class="fa fa-file-o"></i>&nbsp;&nbsp;<a href="<?php echo $ch . $page; ?>.gexf" title="Download this graph (GEXF, readable with Gephi)">network</a>&nbsp;&nbsp;<i class="fa fa-files-o"></i>&nbsp;&nbsp;<a href="<?php echo $ch; ?>.zip" title="Download all <?php echo $chamber; ?> graphs (GEXF, readable with Gephi)">full series</a></p>
    <p><a href="#" id="recenter-camera" class="button">reset zoom</a>&nbsp;&nbsp;<a href="#" id="toggle-layout" class="button">Animate</a> <small><a href="https://gephi.org/2011/forceatlas2-the-new-version-of-our-home-brew-layout/" title="Details on the Force Atlas 2 algorithm."><i class="fa fa-info-circle"></i></a></small></p>
    <footer>
      <p>Inspired by <a href="http://coulmont.com/blog/2011/09/02/travail-de-deputes/">Baptiste&nbsp;Coulmont</a> and <a href="http://jhfowler.ucsd.edu/cosponsorship.htm">James&nbsp;Fowler</a>, built with <a href="http://gexf.net/format/" title="GEXF file format (Gephi)">GEXF</a>, <a href="http://www.r-project.org/" title="The R Project for Statistical Computing">R</a> and <a href="http://sigmajs.org/" title"JavaScript library dedicated to graph drawing">sigma.js</a>. See <a href="https://github.com/briatte/neta/">this repo</a> for the code and data. 
      Background photo from <?php if($ch == "an") echo "<a href='http://commons.wikimedia.org/wiki/File:Panorama_de_l%27h%C3%A9micyle_de_l%27assembl%C3%A9e_nationale.jpg' title='Original photograph by Richard Ying and Tangui Morlier'>"; else echo "<a href='https://commons.wikimedia.org/wiki/File:L%27h%C3%A9micycle_du_S%C3%A9nat_fran%C3%A7ais_en_septembre_2009.jpg' title='Original photograph by Romain Vincens'>"; ?>Wikimedia</a>.</p>
    </footer>
    <div id="graph-container"></div>
  </div>
  <div id="caption" class="bg_<?php echo $ch; ?>">
    <?php echo $caption; ?>
  </div>

</div>

<script src="sigmajs-release-v1.0.2/sigma.min.js"></script>
<script src="sigmajs-release-v1.0.2/plugins/sigma.parsers.gexf.min.js"></script>
<script src="sigmajs-release-v1.0.2/plugins/sigma.layout.forceAtlas2.min.js"></script>

<script>
function decimalAdjust(type, value, exp) {
	// If the exp is undefined or zero...
	if (typeof exp === 'undefined' || +exp === 0) {
		return Math[type](value);
	}
	value = +value;
	exp = +exp;
	// If the value is not a number or the exp is not an integer...
	if (isNaN(value) || !(typeof exp === 'number' && exp % 1 === 0)) {
		return NaN;
	}
	// Shift
	value = value.toString().split('e');
	value = Math[type](+(value[0] + 'e' + (value[1] ? (+value[1] - exp) : -exp)));
	// Shift back
	value = value.toString().split('e');
	return +(value[0] + 'e' + (value[1] ? (+value[1] + exp) : exp));
}

// Decimal round
if (!Math.round10) {
	Math.round10 = function(value, exp) {
		return decimalAdjust('round', value, exp);
	};
}

// Add a method to the graph model that returns an
// object with every neighbors of a node inside:
sigma.classes.graph.addMethod('neighbors', function(nodeId) {
  var k,
      neighbors = {},
      index = this.allNeighborsIndex[nodeId] || {};

  for (k in index)
    neighbors[k] = this.nodesIndex[k];

  return neighbors;
});

sigma.classes.graph.addMethod('getNeighborsCount', function(nodeId) {
  return this.allNeighborsCount[nodeId];
});

sigma.parsers.gexf(
  document.title.replace("Assemblée nationale", "an").replace("Sénat", "se").replace(", législature ", "").replace("Cosponsorship networks in the French Parliament: ", "")+'.gexf',
  { // Here is the ID of the DOM element that
    // will contain the graph:
    container: 'sigma-container'
  },
  function(s) {

    // We first need to save the original colors of our
    // nodes and edges, like this:
    s.graph.nodes().forEach(function(n) {
      n.originalColor = n.color;
      n.originalX = n.x;
      n.originalY = n.y;
    });
    s.graph.edges().forEach(function(e) {
      e.originalColor = e.color;
    });

    // When a node is clicked, we check for each node
    // if it is a neighbor of the clicked one. If not,
    // we set its color as grey, and else, it takes its
    // original color.
    // We do the same for the edges, and we only keep
    // edges that have both extremities colored.
    s.bind('clickNode', function(e) {
      var nodeId = e.data.node.id,
          toKeep = s.graph.neighbors(nodeId);
      toKeep[nodeId] = e.data.node;

      s.graph.nodes().forEach(function(n) {
        if (toKeep[n.id])
          n.color = n.originalColor;
        else
          n.color = '#555';
      });

      s.graph.edges().forEach(function(e) {
        if (toKeep[e.source] && toKeep[e.target])
          e.color = e.originalColor;
        else
          e.color = '#333';
      });
            
      // append mandate suffix
      //
      var mandats = e.data.node.attributes['nb_mandats'];
      if (mandats == "1")
        mandats = "first";
      else if (mandats == "2")
        mandats = "second"
      else if (mandats == "3")
        mandats = "third"
      else
        mandats = mandats + 'th';
      
      // hack alpha value
      //
      var rgba = e.data.node.color;
      // rgba = rgba.replace('0.3)', '1)')
        
      // explicit party groups
      //
      group = e.data.node.attributes['party'];
      party = "?";
      if(group == "CEN") party = "Centrists/rightwing";
      if(group == "COM") party = "Communists/far-left";
      if(group == "SOC") party = "Socialists/leftwing";
      if(group == "DRO") party = "rightwing";
      if(group == "FN") party = "Front National/far-right";
      if(group == "RAD") party = "Radicals/leftwing";
      if(group == "ECO") party = "Greens/leftwing";
      if(group == "SE") party = "unaffiliated";
      
      profile = "<a href='" + e.data.node.attributes['url'] + "' title='Go to profile (<?php echo $chamber; ?>, new window)' target='_blank'>"
      
      if(document.title.match('Assemblée nationale'))
        document.getElementById('caption').innerHTML = '<p style="background:' + rgba + ';">' + profile + '<img src="' + e.data.node.attributes['url'].replace("http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=", "http://www.assemblee-nationale.fr/sycomore/biographies/photo/jpg/") + '.jpg" alt="no photo available" /></a> You selected ' + profile + e.data.node.label + '</a> <span title="Political party affiliation(s): ' + group.replace('/', ", ") + '" style="color:' + rgba.replace('0.3)', '1)') + ';">(' + party + ')</span>, an <acronym title="Member of Parliament">MP</acronym> from <a href="https://fr.wikipedia.org/wiki/' + e.data.node.attributes['nom_circo'] + '" title="Search on Wikipedia Francophone">' + e.data.node.attributes['nom_circo'] + '</a> who <?php echo $be; ?> serving a ' + mandats + ' term, and who <?php echo $have; ?> <span title="unweighted Freeman degree">' + s.graph.getNeighborsCount(nodeId) + ' cosponsors</span> during the legislature. The <a href="http://toreopsahl.com/tnet/weighted-networks/shortest-paths/">mean weighted distance</a> between this MP and all others <?php echo $be; ?> around&nbsp;' + Math.round10(e.data.node.attributes['distance'], -1) + '.</p>';
      else
        document.getElementById('caption').innerHTML = '<p style="background:' + rgba + ';">' + profile + '<img src="' + e.data.node.attributes['url'].replace("http://www.senat.fr/senateur/", "http://www.senat.fr/senimg/").replace(".html", ".jpg") + '" alt="no photo available" /></a> You selected ' + profile + e.data.node.label + '</a> <span title="Political party affiliation(s): ' + group.replace('/', ", ") + '" style="color:' + rgba.replace('0.3)', '1)') + ';">(' + party + ')</span>, a senator from <a href="https://fr.wikipedia.org/wiki/' + e.data.node.attributes['nom_circo'] + '" title="Search on Wikipedia Francophone">' + e.data.node.attributes['nom_circo'] + '</a> who <?php echo $be; ?> serving a ' + mandats + ' term, and who <?php echo $have; ?> <span title="unweighted Freeman degree">' + s.graph.getNeighborsCount(nodeId) + ' cosponsors</span> during the legislature. The <a href="http://toreopsahl.com/tnet/weighted-networks/shortest-paths/">mean weighted distance</a> between this senator and all others <?php echo $be; ?> around&nbsp;' + Math.round10(e.data.node.attributes['distance'], -1) + '.</p>';
      
      // Since the data has been modified, we need to
      // call the refresh method to make the colors
      // update effective.
      s.refresh();
    });

    // When the stage is clicked, we just color each
    // node and edge with its original color.
    s.bind('clickStage', function(e) {
      s.graph.nodes().forEach(function(n) {
        n.color = n.originalColor;
      });

      s.graph.edges().forEach(function(e) {
        e.color = e.originalColor;
      });

      // Same as in the previous event:
      s.refresh();
      
      document.getElementById('caption').innerHTML = '<?php echo $caption; ?>';
    });
    
    // console.log('weighting edges');
    // s.graph.edges().forEach(function(e){
    //   e.size = e.inv;
    // });
    
    s.settings({
      // edgeColor: 'default',
      // defaultEdgeColor: '#555',
      minNodeSize: 2,
      maxNodeSize: 6,
      defaultLabelColor: '#fff',
      defaultLabelSize: 18,
      font: "source sans pro",
      // zoomMin = .3,
      minEdgeSize: .1,
      maxEdgeSize: .1,
      labelHoverBGColor: 'node',
      defaultLabelHoverColor: '#fff',
      labelHoverShadow: 'node'
    });
    
    // Refresh the graph to see the changes:
    s.refresh();
    
    // hide edges
    //
    document.getElementById('showEdges').addEventListener('change',
    function(e){
      if (e.target.checked) {
        s.settings({
          drawEdges: false
        });
      } else {
        s.settings({
          drawEdges: true
        });
      }
      s.refresh();
    });
    
    // hide labels
    //
    document.getElementById('showLabels').addEventListener('change', 
    function(e){
      if (e.target.checked) {
        s.settings({
          drawLabels: false
        });
      } else {
        s.settings({
          drawLabels: true
        });
      }
      s.refresh();
    }); 
    
    // hide sparse ties
    //
    document.getElementById('showSparse').addEventListener('change', 
    function(e){
      if (e.target.checked) {
        s.graph.edges().forEach(function(e) {
          if(e.weight > 0.5)
            e.color = e.originalColor;
          else
            e.color = '#333';
        });
        s.settings({
          minEdgeSize: 1,
          maxEdgeSize: 1
        });
      } else {
        s.graph.edges().forEach(function(e) {
          e.color = e.originalColor;
        });
        s.settings({
          minEdgeSize: .1,
          maxEdgeSize: .1,
        });
      }
      s.refresh();
    }); 
        
    // view as map
    //
    document.getElementById('showMap').addEventListener('change', 
    function(e){
      if (e.target.checked) {
        s.graph.nodes().forEach(function(n) {
          
          // map node position to lon/lat
          n.x = n.attributes['lon'];
          n.y = n.attributes['lat'] * -1;
                  
        });
      } else {
        s.graph.nodes().forEach(function(n) {
          n.x = n.originalX;
          n.y = n.originalY;
        });
      }
      s.refresh();
    }); 
    
    // force atlas
    //
    document.getElementById('toggle-layout').addEventListener('click', 
    function() {
      if ((s.forceatlas2 || {}).isRunning) {
        s.stopForceAtlas2();
        document.getElementById('toggle-layout').innerHTML = 'Animate';
      } else {
        s.startForceAtlas2();
        document.getElementById('toggle-layout').innerHTML = 'Stop';
      }
    });
    
    // reset zoom
    document.getElementById('recenter-camera').addEventListener('click', 
    function() {
      s.cameras[0].goTo({
                x: 0,
                y: 0,
                angle: 0,
                ratio: 1
              });
    });
    
  }
);
</script>

</body>
</html>
