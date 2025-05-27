var mathInputBinding = new Shiny.InputBinding();

$.extend(mathInputBinding, {

  initialize: function(el) {
    el.mathFieldElement = document.getElementById(el.id);

    el.mathField = MQ.MathField(el.mathFieldElement, {
      handlers: {
        edit: function() {
          el.value = el.mathField.latex();
        }
      }
    });
  },

  // find inputs
  find: function(scope) {
    return $(scope).find(".mathquill-editable");
  },

  getId: function(el) {
    return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
  },

  // retrieve value
  getValue: function(el) {
    el.value = el.mathField.latex();
    return el.value;
  },

  // set values
  setValue: function(el, value) {
    el.value = el.mathField.latex(value);
  },

  // handle messages from the server
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    this._updateLabel(data.label, this._getLabelNode(el));

    $(el).trigger('change');
  },

  subscribe: function(el, callback) {
    $(el).on('input.mathInputBinding', function(event) {
      callback(true);
    });
    $(el).on('change.mathInputBinding', function(event) {
      callback(false);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.mathInputBinding');
  },

  // the same as shiny's textInput
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  },

  _getLabelNode: function(el) {
    return $(el).parent().find('label[for="' + escape(el.id) + '"]');
  },

  _updateLabel: function(a, b) {
    if ("undefined" != typeof a) {
      if (1 !== b.length)
          throw new Error("labelNode must be of length 1");
      var c = $.isArray(a) && 0 === a.length;
      c ? b.addClass("shiny-label-null") : (b.text(a),
      b.removeClass("shiny-label-null"));
    }
  }

});

Shiny.inputBindings.register(mathInputBinding, 'shinymath.mathInput');
