var slowTextInputBinding = new Shiny.InputBinding();
$.extend(slowTextInputBinding, {
  find: function (scope) {
    return $(scope).find('.shiny-slow-text');
  },
  getId: function (el) {
    return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
  },
  getValue: function (el) {
    return el.value;
  },
  setValue: function (el, value) {
    el.value = value;
  },
  subscribe: function (el, callback) {
    $(el).on('keyup.slowTextInputBinding input.slowTextInputBinding', function (event) {
      callback(true);
    });
    $(el).on('change.slowTextInputBinding', function (event) {
      callback(false);
    });
  },
  unsubscribe: function (el) {
    $(el).off('.slowTextInputBinding');
  },
  receiveMessage: function (el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    updateLabel(data.label, this._getLabelNode(el));

    if (data.hasOwnProperty('placeholder'))
      el.placeholder = data.placeholder;

    $(el).trigger('change');
  },
  getState: function (el) {
    return {
      label: this._getLabelNode(el).text(),
      value: el.value,
      placeholder: el.placeholder
    };
  },
  getRatePolicy: function (el) {
    return {
      policy: 'debounce',
      delay: $(el).data('debounce')
    };
  },
  _getLabelNode: function (el) {
    return $(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
});
Shiny.inputBindings.register(slowTextInputBinding, 'blockr.slowTextInputBinding');
Shiny.inputBindings.setPriority('blockr.slowTextInputBinding', 10);