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

var Translations = function(config) {
  var cellLength = config.cellLength;

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

var Config = function(gridWidth, gridHeight) {
  this.cellLength = 20;
  this.canvasWidth = this.cellLength * gridWidth;
  this.canvasHeight = this.cellLength * gridHeight;
};

var Drawing = function(canvas, config) {
  canvas.width = config.canvasWidth;
  canvas.height = config.canvasHeight;
  var ctx = canvas.getContext('2d');
  ctx.fillStyle = "rgb(200,0,0)";
  var translations = new Translations(config);

  var drawAliveCell = function(cell) {
    var cornerLocation = translations.coordinatesForCell(cell);
    ctx.fillRect(cornerLocation.xPixels, cornerLocation.yPixels, config.cellLength, config.cellLength);
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
    for(var xPixels = 0; xPixels <= config.canvasWidth; xPixels += config.cellLength) {
      drawVerticalLine(xPixels);
    }

    for(var yPixels = 0; yPixels <= config.canvasHeight; yPixels += config.cellLength) {
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

var init = function(worldID, gridWidth, gridHeight) {
  var config = new Config(gridWidth, gridHeight);
  var localState = new LocalState();
  var canvas = document.getElementById('world');
  var drawing = new Drawing(canvas, config);
  var translations = new Translations(config);
  var tick = new Audio('http://soundbible.com/grab.php?id=2044&type=mp3');
  var turnOnSound = new Audio('http://soundbible.com/grab.php?id=1821&type=mp3');
  var turnOffSound = new Audio('http://soundbible.com/grab.php?id=1610&type=mp3');
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
    drawing.refresh(localState.aliveCells);
  }

  var updateIsRunning = function(running) {
    var wasRunning = localState.running;
    localState.running = running;
    var turnsOn = localState.running && !wasRunning;
    var turnsOff = !localState.running && wasRunning;
    if (turnsOn) {
      turnOnSound.play()
    }
    if (turnsOff) {
      turnOffSound.play()
    }
    $("#status-running").toggle(localState.running);
    $("#status-stopped").toggle(!localState.running);
    $("#pause").toggle(localState.running);
    $("#resume").toggle(!localState.running);
  }

  connection.onmessage = function(message){
    var parsed = JSON.parse(message.data);
    var cells = $.map(parsed.uAliveCells, function(c){
      return new Cell(c.pX, c.pY);
    });
    var isChange = cellCollectionIsChange(cells);
    updateLocalCells(cells);
    updateIsRunning(parsed.uRunning);
    if (isChange) {
      tick.play()
    }
  };

  var cellCollectionIsChange = function(newCells) {
    if (newCells.length != localState.aliveCells.length) {
      return true;
    }
    for (var i=0; i<newCells.length; i++) {
      if (!localState.cellAlive(newCells[i])){
        return true;
      }
    }
    return false;
  }

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

