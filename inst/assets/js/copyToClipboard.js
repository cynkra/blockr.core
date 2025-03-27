function copyCode(id, fallback = false) {

  var el = document.getElementById(id);

  if (el.innerHTML === "") {
    window.Shiny.notifications.show(
      {
        html: "<span>No code to copy.</span>",
        type: "warning"
      }
    );
    return;
  }

  if (!navigator.clipboard || !fallback) {

    var range = document.createRange();
    range.selectNode(el);

    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);

    try {

      var successful = document.execCommand("copy");
      clipboardMessage(successful);

    } catch (error) {

      clipboardError(error.message);

    } finally {

      window.getSelection().removeAllRanges();
    }

  } else {

    navigator.clipboard.writeText(
      htmlDecode(el.innerHTML)
    ).then(
      () => {
        clipboardMessage(true);
      }
    ).catch(
      (error) => {
        console.log(error.message);
        copyCode(id, true);
      }
    );
  }
}

function htmlDecode(input) {
  var doc = new DOMParser().parseFromString(input, "text/html");
  return doc.documentElement.textContent;
}

function clipboardError(msg) {
  window.Shiny.notifications.show(
    {
      html: "<span>Clipboard error: " + msg + "</span>",
      type: "error"
    }
  );
}

function clipboardMessage(success) {

  var msg = success ? "successful" : "unsuccessful";

  window.Shiny.notifications.show(
    {
      html: "<span>Code copy to clipboard was " + msg + ".</span>",
      type: success ? "message" : "error"
    }
  );
}
