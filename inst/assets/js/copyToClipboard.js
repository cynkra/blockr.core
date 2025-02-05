function copyText(text) {
  return new Promise((resolve, reject) => {
    if (typeof navigator !== "undefined" && typeof navigator.clipboard !== "undefined" && navigator.permissions !== "undefined") {
      const type = "text/plain";
      const blob = new Blob([text], { type });
      const data = [new ClipboardItem({ [type]: blob })];
      navigator.permissions.query({name: "clipboard-write"}).then((perm) => {
        if (perm.state === "granted" || perm.state === "prompt") {
          navigator.clipboard.write(data).then(resolve, reject).catch(reject);
        }
        else {
          reject(new Error("Permission to copy not granted."));
        }
      });
    } else {
      reject(new Error("Copying not supported by this browser."));
    }
  });
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
    await copyText(msg.code.trim());
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
