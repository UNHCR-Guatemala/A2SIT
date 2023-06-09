create_guide <- function(id){

  cicerone::Cicerone$
    new(opacity = 0.5)$
    step(
      el = "db_sidebar",
      title = "The sidebar",
      position = "right-center",
      description = paste0("The sidebar lets you navigate through the app: clicking the links will take you to the different ",
                           "app screens. Generally, you should work from the top downwards. Start with data input (where you can ",
                           "download a template for your data). Click the button at the top to minimise the sidebar.")
    )$
    step(
      el = "header_help_icon",
      title = "Tab help",
      position = "bottom",
      description = paste0("Click on this help icon in any tab in the app to get instructions and tips for that tab. This includes ",
                           "links to the full A2SIT documentation. Also look out for blue help icons which give quick tips when you hover over them.")
    )$
    step(
      el = "save_session",
      title = "Save session",
      position = "bottom",
      description = paste0("Click here to save your session so you can return later. (Note: to modify depending on how sessions are saved/loaded).")
    )$
    step(
      el = "load_session",
      title = "Load session",
      position = "bottom",
      description = paste0("If you have previously saved your session click here to load a previous session and pick up where you left off. (Note: to modify depending on how sessions are saved/loaded).")
    )$
    step(
      el = "export_to_excel",
      title = "Export",
      position = "bottom",
      description = paste0("Once you have uploaded your data, you can click here at any point to download the results so far, either to Excel or to R. Note that index scores are only calculated once you click on the 'Results' tab.")
    )$
    step(
      el = "id_welcome-go_to_doc",
      title = "A2SIT documentation",
      position = "top",
      description = paste0("Click here to read the full A2SIT documentation. You may wish to keep this open as you work through the app.")
    )$
    step(
      el = "id_welcome-go_to_datainput",
      title = "Data input",
      position = "top",
      description = paste0("If you're ready to get started, click here to go to the data upload tab. Remember to use the help icons in the app if you need to!")
    )

}
