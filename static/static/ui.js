var Cell = function(pX, pY) {
  this.pX = pX;
  this.pY = pY;
  this.equals = function(other) {
    return (this.pX == other.pX ) && (this.pY == other.pY);
  }
  return this;
}
var LocalState = function() {
  this.aliveCells = [];
  this.running = undefined;
  this.cellAlive = function(candidate) {
    var finds = $.grep(this.aliveCells, function(cell) {
      return candidate.equals(cell);
    });
    return !!(finds.length);
  }
  this.addCell = function(cell) {
    this.aliveCells.push(cell);
  }
  this.removeCell = function(cell) {
    this.aliveCells = $.grep(this.aliveCells, function(alive) {
      return !cell.equals(alive);
    });
  }
  this.toggleCell = function(cell) {
    if (this.cellAlive(cell)) {
      this.removeCell(cell);
    } else {
      this.addCell(cell);
    }
  };
  this.stomp = function(cells) {
    this.aliveCells = cells;
  }
}

var Translations = function() {
  var cellLength = Config.cellLength;

  var cellForCoordinates = function(x, y) {
    return new Cell(Math.floor(x / cellLength), Math.floor(y / cellLength));
  }

  this.coordinatesForCell = function(indices) {
    return {
      xPixels: Math.floor(indices.pX * cellLength),
      yPixels: Math.floor(indices.pY * cellLength),
    }
  }

  this.cellForClick = function(e, canvas) {
    var xCoordinateInCanvas = e.pageX - canvas.offsetLeft;
    var yCoordinateInCanvas = e.pageY - canvas.offsetTop;
    return cellForCoordinates(xCoordinateInCanvas, yCoordinateInCanvas);
  }
}

var Config = {
  canvasWidth: 400,
  canvasHeight: 400,
  cellLength: 20
};

var Drawing = function(canvas) {
  canvas.width = Config.canvasWidth;
  canvas.height = Config.canvasHeight;
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = "rgb(200,0,0)";
  var translations = new Translations();

  var drawAliveCell = function(cell) {
    var cornerLocation = translations.coordinatesForCell(cell);
    ctx.fillRect(cornerLocation.xPixels, cornerLocation.yPixels, Config.cellLength, Config.cellLength);
  }

  var drawVerticalLine = function(x) {
    ctx.beginPath();
    ctx.moveTo(x, 0);
    ctx.lineTo(x, canvas.height);
    ctx.stroke();
  }

  var drawHorizontalLine = function(y) {
    ctx.beginPath();
    ctx.moveTo(0, y);
    ctx.lineTo(canvas.width, y);
    ctx.stroke();
  }

  var drawGrid = function() {
    for(var xPixels = 0; xPixels <= canvas.width; xPixels += Config.cellLength) {
      drawVerticalLine(xPixels);
    }

    for(var yPixels = 0; yPixels <= canvas.height; yPixels += Config.cellLength) {
      drawHorizontalLine(yPixels);
    }
  }
  var clear = function(){ ctx.clearRect(0, 0, canvas.width, canvas.height);}
  drawAliveCells = function(cells) {
    for (var i = 0; i < cells.length; i++){
        var cell = cells[i];
        drawAliveCell(cell);
    }
  }
  this.refresh = function(cells){
    clear();
    drawAliveCells(cells);
    drawGrid();
  }
}

var Net = function(connection) {
  this.setAlive = function(cells){
    var message = {tag: "SetAlive", contents: cells};
    connection.send(JSON.stringify(message));
  }
  this.setDead = function(cells){
    var message = {tag: "SetDead", contents: cells};
    connection.send(JSON.stringify(message));
  }
  this.pause = function(){
    var message = {tag: "Pause", contents: []};
    connection.send(JSON.stringify(message));
  }
  this.resume = function(){
    var message = {tag: "Resume", contents: []};
    connection.send(JSON.stringify(message));
  }
  this.withSavedGame = function(name, callback) {
    $.get('/worlds/'+name, function(stuff) {
      callback(stuff);
      console.log(stuff);
    });
  }
  this.saveGame = function(name, cells) {
    $.post('/worlds/'+name, JSON.stringify(cells))
  }
}

var init = function(worldID) {
  var localState = new LocalState();
  var canvas = document.getElementById('world');
  var drawing = new Drawing(canvas);
  var translations = new Translations();
  var audio = new Audio('/static/tick.mp3');
  connection = new WebSocket("ws://localhost:9160/?worldID=" + worldID);
  var net = new Net(connection);
  connection.onclose = function(e) {
    console.log(e.code)
  };

  var toggleCell = function(cell) {
    var sendToggle = function() {
      if (localState.cellAlive(cell)) {
        net.setDead([cell]);
      } else {
        net.setAlive([cell]);
      }
    }
    sendToggle()
    localState.toggleCell(cell)
    drawing.refresh(localState.aliveCells);
  }

  canvas.addEventListener('click', function(e){
    clickedCell = translations.cellForClick(e, canvas);
    toggleCell(clickedCell);
  });

  var updateLocalCells = function(cells) {
    localState.stomp(cells);
    drawing.refresh(cells);
  }

  var updateRunning = function(running) {
    localState.running = running;
    $("#status-running").toggle(localState.running);
    $("#status-stopped").toggle(!localState.running);
  }

  connection.onmessage = function(message){
    var parsed = JSON.parse(message.data);
    updateLocalCells(parsed.uAliveCells);
    updateRunning(parsed.uRunning);
    if (localState.running) {
      audio.play()
    }
  };

  var clear = function() {
    net.setDead(localState.aliveCells);
    updateLocalCells([]);
  }

  $("#pause").on('click', function() {
    net.pause()
  });

  $("#resume").on('click', function() {
    net.resume()
  });

  $("#restore_button").on('click', function() {
    net.withSavedGame($("#restore_name").val(), function(cells){
      clear();
      net.setAlive(cells);
      updateLocalCells(cells);
    });
  });

  $("#clear_button").on('click', function() {
    clear();
  });

  $("#save_button").on('click', function() {
    net.saveGame($("#save_name").val(), localState.aliveCells);
  });
}

