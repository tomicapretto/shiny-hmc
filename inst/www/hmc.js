const drawTrajectoryHMC = function (message) {
  var id = message.id;

  // The IDs of the objects that make up the trajectory
  var objects = message.objects;

  // The ID of the subscene
  var subscene = message.subscene;

  // The ID of the input that will be changed once the trajectory finished
  var completed_id = message.completed_id;

  // Total time for the animation (target)
  var total_time = message.total_time;

  // The plot/widget
  var rgl = document.getElementById(id).rglinstance;

  // IDs of objects to add
  var objs_keys_array = Object.keys(objects).map((key) => {
    return parseInt(key, 10);
  });

  // IDs of segments, which will be deleted
  var segments_keys_array = objs_keys_array.slice(0, -1);

  // Time interval between addition of segments
  var interval = total_time / segments_keys_array.length;

  // First do a pass to init all the segments, before drawing.
  // Then do another pass where we add segments to subscene and draw them.
  // All the overhead of object initialization happens together at the beginning.
  for (i = 0; i < segments_keys_array.length; i++) {
    key = segments_keys_array[i];
    var object = objects[key];
    rgl.scene.objects[key] = object;
    rgl.initObj(object);
  }

  // If we want to change the colors...
  // 0. Get object ID
  // obj_id = segments_keys_array[i];
  // 1. Get object with its ID
  // obj = rgl.getObj(obj_id);
  // 2. Change color
  // obj.colors[0] = new_color;
  // 3. Initialize again
  // rgl.initObjId(obj_id)
  // 4. Draw scene
  // rgl.drawScene();

  function* animate() {
    // Add segments
    for (i = 0; i < segments_keys_array.length; i++) {
      key = segments_keys_array[i];
      rgl.addToSubscene(key, subscene);
      rgl.drawScene();
      yield interval;
    }

    // Add point
    key = objs_keys_array.slice(-1)[0];
    var object = objects[key];
    rgl.scene.objects[key] = object;
    rgl.initObj(object);
    rgl.addToSubscene(key, subscene);
    rgl.drawScene();
    yield interval;

    // Remove segments
    for (i = 0; i < segments_keys_array.length; i++) {
      rgl.delFromSubscene(segments_keys_array[i], subscene);
      delete rgl.scene.objects[segments_keys_array[i]];
    }
    rgl.drawScene();
    sendCompletedMessage(completed_id);
  }

  // Call animate generator
  iterate_with_pauses(this, animate());
};

Shiny.addCustomMessageHandler("drawTrajectoryHMC", drawTrajectoryHMC);

const deleteFromRglPlot = function (message) {
  var id = message.id;

  // The IDs of the objects to delete
  var objects = message.objects;

  // The ID of the subscene where we delete things from
  var subscene = message.subscene;

  // The plot/widget
  var rgl = document.getElementById(id).rglinstance;

  for (i = 0; i < objects.length; i++) {
    rgl.delFromSubscene(objects[i], subscene);
    delete rgl.scene.objects[objects[i]];
  }
  rgl.drawScene();
};

Shiny.addCustomMessageHandler("deleteFromRglPlot", deleteFromRglPlot);

const addToRglPlot = function (message) {
  var id = message.id;

  // The IDs of the objects to add
  var objects = message.objects;

  // The ID of the subscene where we delete things from
  var subscene = message.subscene;

  // The plot/widget
  var rgl = document.getElementById(id).rglinstance;

  // IDs of objects to add
  var objs_keys_array = Object.keys(objects).map((key) => {
    return parseInt(key, 10);
  });

  // Add objects
  for (i = 0; i < objs_keys_array.length; i++) {
    key = objs_keys_array[i];
    var object = objects[key];
    rgl.scene.objects[key] = object;
    rgl.initObj(object);
    rgl.addToSubscene(key, subscene);
  }

  // Draw scene
  rgl.drawScene();
};

Shiny.addCustomMessageHandler("addToRglPlot", addToRglPlot);

function iterate_with_pauses(context, iterator) {
  // animation_id allows only one iterating process to exist within single context
  // new 'process' has priority and automatically stops previous
  let animation_id = Date.now();
  context.animation_id = animation_id;

  function iterate(iterator) {
    if (context.animation_id != animation_id) {
      return;
    }
    let { value, done } = iterator.next();
    if (!done) {
      setTimeout(() => {
        iterate(iterator);
      }, value);
    } else {
      context.animation_id = null;
    }
  }
  // run iteration
  iterate(iterator);
}

function sendCompletedMessage(input_name) {
  // We are interested in the existence of the message, not the value
  // so it's a random value which is unused
  var message = { value: Math.random() };
  Shiny.onInputChange(input_name, message);
}

function updateInputValue(message) {
  // Here we're also interested in the existence of the update, not the value.
  var input_name = message.id;
  var message = { value: Math.random() };
  Shiny.onInputChange(input_name, message);
}

Shiny.addCustomMessageHandler("updateInputValue", updateInputValue);

// Adds the "p" elements to the plot where we put HMC statistics
function initializeHMCStatistics(plot_id) {
  var div = document.createElement("div");
  div.id = "text_div";
  document.getElementById(plot_id).appendChild(div);

  var p_position = document.createElement("p");
  p_position.id = "text_position";
  document.getElementById("text_div").appendChild(p_position);

  var p_momentum = document.createElement("p");
  p_momentum.id = "text_momentum";
  document.getElementById("text_div").appendChild(p_momentum);

  var p_status = document.createElement("p");
  p_status.id = "text_status";
  document.getElementById("text_div").appendChild(p_status);

  var p_h_diff = document.createElement("p");
  p_h_diff.id = "text_h_diff";
  document.getElementById("text_div").appendChild(p_h_diff);

  var p_divergent = document.createElement("p");
  p_divergent.id = "text_divergent";
  document.getElementById("text_div").appendChild(p_divergent);
}

// Call a function after making sure the canvas exist
function waitForCanvas(plot_id, callback) {
  var interval = setInterval(function () {
    // Check if the containers have the canvas element
    var cv1 = $("#" + plot_id)[0].querySelector("canvas") != null;

    if (Boolean(cv1)) {
      clearInterval(interval);
      callback();

      // Change the height of the 'div' so it's adjusts to canvas height
      // Can't do this when creating the plot because rgl does not accept relative units
      $(".rglWebGL").each(function (obj) {
        $(this).css("height", "100%");
      });
    }
  }, 250);
}

// We call 'initializeHMCStatistics' only after we know the canvases exist
function syncCanvas(message) {
  waitForCanvas("rglPlot", function () {
    initializeHMCStatistics("rglPlot");
  });
}

// When the 'rglPlot' output changes, we start the listeners
$(document).on("shiny:value", function (event) {
  if (event.target.id === "rglPlot") {
    syncCanvas(null);
  }
});
