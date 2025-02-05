function copyText(text) {
  if (typeof ClipboardItem && navigator.clipboard.write) {
    const data = new ClipboardItem({
      "text/plain": new Promise(async (resolve) => {
        resolve(new Blob([text], { type: "text/plain" }))
      })
    })
    navigator.clipboard.write([data]);
  } else {
    navigator.clipboard.writeText(text);
  }
}

window.Shiny.addCustomMessageHandler("blockr-copy-code", (msg) => {

  if (!msg.code) {
    window.Shiny.notifications.show({
      html: "<span>No code found to copy.</span>",
      type: "error",
    });
    return;
  }

  try {
    copyText(msg.code.trim());
    window.Shiny.notifications.show({
      html: "<span>Code successfully copied to clipboard.</span>",
      type: "message",
    });
  } catch (error) {
    window.Shiny.notifications.show({
      html: "<span>" + error.message + "</span>",
      type: "error"
    });
  }
});
