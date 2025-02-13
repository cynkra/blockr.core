Shiny.addCustomMessageHandler(
   "move-block-ui",
   (m) => {
      $(m.dest).append($(m.sel));
   }
);
