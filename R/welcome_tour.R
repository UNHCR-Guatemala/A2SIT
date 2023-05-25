create_guide <- function(id){

  cicerone::Cicerone$
    new(opacity = 0.5)$
    step(
      el = "db_sidebar",
      title = "The sidebar",
      position = "right-center",
      description = "The sidebar lets you navigate through the app: clicking the links will take you to the different app screens. Generally, you should work from the top downwards."
    )$
    step(
      el = "welcome_sb_link",
      title = "Welcome page",
      position = "right-center",
      description = "This link takes you to the welcome page, where you are right now!"
    )$
    step(
      el = "upload_sb_link",
      title = "Data upload",
      position = "right-center",
      description = "This link takes you to the page where you can upload your data."
    )$
    step(
      el = "analyse_sb_link",
      title = "Indicator analysis",
      position = "right-center",
      description = "This page lets you analyse your indicator data and optionally remove indicators."
    )$
    step(
      el = "results_sb_link",
      title = "Results",
      position = "right-center",
      description = "The results page shows the index results, as maps, bar charts and tables."
    )

}
