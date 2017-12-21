function startPreGame(code){
  var url = "ws://" + location.host + (code ? "/g/" + code : "/new");
  var ws = new WebSocket(url);
  if(code){
    ws.onopen = function(m){
      InitPreGameState(code,ws);
    }
  } else {
    ws.onmessage = function(m){
      InitPreGameState(m.data, ws);
    }
  }
}

function InitPreGameState(code, ws){
  var Players = [];
  var PlayerCount = 0;
  var Name = null;

  var name = document.createElement("INPUT");
  name.type = "text"; name.id = "name"
  var namelbl = document.createElement("LABEL");
  namelbl.htmlFor = name.id; namelbl.innerHTML = "Your name: ";
  var go = document.createElement("BUTTON");
  go.innerHTML = "Go!";
  var join = document.createElement("DIV");
  join.innerHTML = "Game Code (for other players to join): " + code;
  var scores = document.createElement("DIV");
  var b = document.body;
  b.innerHTML = "";
  b.appendChild(namelbl);
  b.appendChild(name);
  b.appendChild(go);
  b.appendChild(join);
  b.appendChild(scores);

  go.onclick = function(){
    var n;
    if(name.value == ""){ alert("You need to have a name"); return;}
    else{ ws.send(n = name.value); }
    var old = ws.onmessage;
    ws.onmessage = function(m){
      ws.onmessage = old;
      if(/^E/.test(m.data)){
        alert("Error: " + m.data.substr(1));
      } else {
        Name = n;
        b.removeChild(namelbl);
        b.removeChild(name);
        b.removeChild(go);
        old && old(m);
      }
    }
  }
  ws.onmessage = function(m){
    switch(m.data.charAt(0)){
      case "E":
      alert("Error: " + m.data.substr(1));
      break;
      case "P":
      PlayerCount = parseInt(m.data.substr(1));
      PlayerScores(scores, Players, PlayerCount, Name);
      break;
      case "S":
      Players = ParseScores(m.data.substr(1));
      PlayerScores(scores, Players, PlayerCount, Name);
      break;
      case "C":
      InitGameState(scores, Players, PlayerCount, Name, ws);
      ws.onmessage(m);
      break;
    }
  }
}

function InitGameState(scores, Players, PlayerCount, Name, ws){
  var Cards = [];
  var Selection = [];
  var cardsTable = document.createElement("TABLE");
  cardsTable.innerHTML="<tbody></tbody>";

  scores.parentNode.insertBefore(cardsTable, scores);

  ws.onmessage = function(m){
    var cardsList;
    var nc;
    switch(m.data.charAt(0)){
      case "E":
      alert("Error: " + m.data.substr(1));
      break;
      case "P":
      PlayerCount = parseInt(m.data.substr(1));
      PlayerScores(scores, Players, PlayerCount, Name);
      break;
      case "S":
      Players = ParseScores(m.data.substr(1));
      PlayerScores(scores, Players, PlayerCount, Name);
      break;
      case "C":
      cardsList = parseCards(m.data.substr(1));
      nc = addCards(Cards, cardsList);
      redrawCards(nc);
      break;
      case "W":
      cardsList = parseCards(m.data.substr(1));
      nc = withdrawCards(Cards, cardsList);
      var news=[];
      for(var i = 0; i<Selection.length; i++){
        if(!cardsList.includes(Selection[i]))news.push(Selection[i]);
      }
      Selection = news;
      redrawCards(nc);
      break;
      case "N":
      alert("Not A Set!");
      break;
    }
  }

  function redrawCards(newCards, updateList){
    var tb = cardsTable.getElementsByTagName("tbody")[0];
    if(tb.childElementCount < 3){tb.innerHTML='<tr></tr><tr></tr><tr></tr>';}
    var rows = tb.childNodes;
    for(var i= 0; i<3; i++){
      var row = rows[i];
      var currentcards = row.childNodes;
      for(var j = 0; j < Math.min(Cards.length, newCards.length); j++){
        var currentCard = Cards[j][i];
        var newCard = newCards[j][i];
        if(currentCard != newCard || (updateList && updateList.includes(currentCard))){
          if(newCard == null){
            currentcards[j].innerHTML='';
            currentcards[j].onclick=function(){};
          } else {
            currentcards[j].innerHTML = drawCard(newCard, Selection.includes(newCard));
            makeClickHandle(currentcards[j], newCard);
          }
        }
      }
      if(Cards.length<newCards.length){
        for(;j<newCards.length;j++){
          var td = document.createElement("TD");
          var newCard = newCards[j][i];
          td.innerHTML = drawCard(newCard, Selection.includes(newCard));
          makeClickHandle(td, newCard);
          row.appendChild(td);
        }
      } else {
        while(currentcards.length>Cards.length){row.removeChild(currentcards[Cards.length]);}
      }
    }
    Cards = newCards;
  }
  
  function makeClickHandle(td, card){
    td.onclick = function(){
      if(Selection.includes(card)){
        td.innerHTML = drawCard(card, false);
        var ns = [];
        for(var i = 0; i< Selection.length; i++) if(Selection[i] != card) ns.push(Selection[i]);
        Selection = ns;
      } else {
        Selection.push(card);
        td.innerHTML = drawCard(card, true);
        maybeSendSet();
      }
    }
  }

  function maybeSendSet(){
    if(Selection.length==3){
      var m = "" + Selection[0] + Selection[1] + Selection[2];
      ws.send(m);
      var s= Selection;
      Selection=[];
      redrawCards(Cards, s);
    }
  }

}

function drawCard(card, selected){
  var number = parseInt(card.charAt(0)) + 1;
  var fill = parseInt(card.charAt(1)); //0: empty; 1: shaded; 2: filled
  var shape = parseInt(card.charAt(2)); //0: roundel; 1: lozenge; 2: squigle
  var colour = parseInt(card.charAt(3)); //0: red; 1: green; 2: purple

  var pre = '<svg xmlns="http://www.w3.org/2000/svg"'
            + ' xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" '
          + 'width="150" height="300" viewBox="0 0 350 700" style="stroke-width:2">'
          + svgdefs
          + '<rect x="0" y="0" height="700" width="350" rx="50" ry="50" fill="none" stroke="'
          + (selected ? "red" : "black")
          +'" />';
  var post = "</svg>";
  var trans;
  switch(number){
    case 1:
    trans = [[75,300]];
    break;
    case 2:
    trans = [[75,500/3],[75,1000/3 + 100]];
    break;
    case 3:
    trans = [[75,100],[75,300],[75,500]];
    break;
  }
  var svg = '';
  for(var i = 0; i<number; i++){
    svg += '<g transform="translate('+trans[i][0]+',' + trans[i][1] + ')">'
         + drawOneShape(shape, colour, fill)
         + '</g>';
  }
  return pre + svg + post;
}


function colour(n){
  switch(n){
    case 0: return "red";
    case 1: return "green";
    case 2: return "purple";
  }
  throw "invalid colour";
}

var svgdefs = '<defs>';
for(var i = 0; i<3; i++){
  svgdefs += '<pattern id="shading'+i+'" patternUnits="userSpaceOnUse" width="10" height="50">'
              + '<path d="M 2,0 l0,50" '
              + 'style="fill:none;stroke-width:5;stroke:' + colour(i) + '"/>'
              + '</pattern>';
}
svgdefs += '</defs>';

function drawShape(shape, style){
  switch(shape){
    case 0:
    return '<rect x="0" y="0" width="200" height="100" rx="50" ry="50" '
           + 'style="' + style + '" />';
    case 1:
    return '<path d="m 0,50 l 100,-50 l 100,50 l -100,50 l -100,-50 z" style="' + style + '"/>'
    case 2:
    return '<rect x="0" y="0" width="200" height="100" style="' + style + '" />';
  }
  throw "invalid shape";
}

function fill(s, col){
  switch(s){
    case 0: return "none";
    case 1: return "url(#shading" + col + ")";
    case 2: return colour(col);
  }
  throw "invalid fill";
}

function drawOneShape(shape, col, fillstyle){
  var style = "stroke:" + colour(col) + ";"
            + "fill:" + fill(fillstyle, col) + ";";
  return drawShape(shape, style);
}

function ParseScores(message){
  var p = [];
  var lines = message.split("\n");
  for(var i = 0; i < lines.length; i++){
    var line = lines[i];
    if(line.length>0){
      var score = parseInt(line);
      var name = line.substr(line.indexOf(" ") + 1);
      p.push({name: name, score: score});
    }
  }
  p.sort(function(a,b){
    if(a.name < b.name){ return -1;}
    if(a.name > b.name){ return 1; }
    return 0;
  });
  return p;
}

function PlayerScores(div, players, count, ownName){
  var html = "<h2>Scores</h2><dl>";
  var normal = "";
  for(var i = 0; i < players.length; i++){
    var p = players[i];
    var term = "</dt><dd>" + p.score + "</dd>"
    if(p.name == ownName){
      html = html + "<dt>You" + term;
    } else {
      normal = normal + "<dt>" + p.name + term;
    }
  }
  var end = '';
  if(players.length < count){
    var left = count - players.length - (ownName == null ? 1 : 0);
    end = "<dt>" + (ownName==null ? "You and " : "" ) +
      left + " other player" + (left == 1 ? "" : "s") + "</dt></dl>";
  }
  div.innerHTML = html + normal + end;
}

function parseCards(message){
  var cards = [];
  for(var i = 0; i < message.length; i+=4) cards.push(message.substr(i,4));
  return cards;
}

function withdrawCards(Cards, list){
  var newCards = Array(Cards.length);
  for(var i = 0; i < Cards.length; i++){
    var c = Cards[i];
    var nc = Array(c.length);
    for(var j = 0; j < c.length; j++){
      nc[j] = (list.includes(c[j]) ? null : c[j]);
    }
    newCards[i] = nc;
  }
  return newCards;
}

function addCards(Cards, list){
  var newCards = Array(Cards.length);
  for(var i = 0; i < Cards.length; i++){
    var c = Cards[i];
    var nc = Array(c.length);
    for(var j = 0; j < c.length; j++){
      if(c[j] == null){
        nc[j] = list.pop();
      } else {
        nc[j] = c[j];
      }
    }
    newCards[i] = nc;
  }
  while(list.length > 0){
    if(list.length >= 3){
      newCards.push(list.splice(0,3));
    }else{
      var a = Array(3);
      a[0] = a[1] = a[2] = null;
      switch(list.length){
        case 2:
        a[1] = list[1];
        case 1:
        a[0] = list[0];
      }
      newCards.push(a);
      list = [];
    }
  }
  return newCards;
}


document.addEventListener("DOMContentLoaded", function(){
  document.getElementById("start").addEventListener("click", function(){
    startPreGame();
  });
  document.getElementById("join").addEventListener("click", function(){
    var id = document.getElementById("game-id").value;
    if(/^[0-9]+$/.test(id)){
      startPreGame(id);
    } else {
      alert("invalid game code");
    }
  });
});