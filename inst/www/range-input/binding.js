const rangeInputBinding = new Shiny.InputBinding();

$.extend(rangeInputBinding, {

  find: function (scope) {
    return $(scope).find('.range-input');
  },

  initialize: function (el) {
    const range = el.querySelector('input');
    const rangeV = el.querySelector('.range-value');

    const newValue = Number((range.value - range.min) * 100 / (range.max - range.min));
    const newPosition = 10 - (newValue * 0.2);    
    
    if (range.dataset["label"] != null) {
      value = range.dataset["label"].split(", ")[range.value - 1]
    } else {
      value = range.value
    }

    rangeV.innerHTML = `<span>${value}</span>`;
    rangeV.style.left = `calc(${newValue}% + (${newPosition}px))`;

    function setValue() {
      const newValue = Number((range.value - range.min) * 100 / (range.max - range.min));
      const newPosition = 10 - (newValue * 0.2);

      if (range.dataset["label"] != null) {
        value = range.dataset["label"].split(", ")[range.value - 1]
      } else {
        value = range.value
      }

      rangeV.innerHTML = `<span>${value}</span>`;
      rangeV.style.left = `calc(${newValue}% + (${newPosition}px))`;
    }

    range.addEventListener('input', setValue);
  },

  getValue: function (el) {
    const input = el.querySelector('input');
    return parseFloat(input.value);
  },

  subscribe: function (el, callback) {
    const controls = el.querySelector('.range-input-controls');
    controls.addEventListener('click', function () {
      callback();
    });
    controls.addEventListener('change', function () {
      callback();
    });
    controls.addEventListener('input', function () {
      callback();
    });
  }

});

Shiny.inputBindings.register(rangeInputBinding);
