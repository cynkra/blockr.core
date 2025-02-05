function copyCode(id) {

  var range = document.createRange();
  range.selectNode(document.getElementById(id));

  window.getSelection().removeAllRanges();
  window.getSelection().addRange(range);

  try {
    var successful = document.execCommand("copy");
    var msg = successful ? "successful" : "unsuccessful";
    window.Shiny.notifications.show({
      html: "<span>Copying code command was " + msg + ".</span>",
      type: successful ? "message" : "error"
    });
  } catch (error) {
    window.Shiny.notifications.show({
      html: "<span>" + error.message + "</span>",
      type: "error"
    });
  } finally {
    window.getSelection().removeAllRanges();
  }
}
