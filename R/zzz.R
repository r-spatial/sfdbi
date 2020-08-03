# nocov start

# setup_repo
if (FALSE) {
  .sfdbi_setup <- function() {
    usethis::create_package("../sfdbi", open = FALSE)
    usethis::use_readme_rmd()
    usethis::use_news_md()
    usethis::use_logo("logo.svg")
    usethis::use_testthat()
    usethis::use_roxygen_md()
    usethis::use_mit_license()
    usethis::use_git()
    #usethis::use_github()
    usethis::use_coverage()
    usethis::use_lifecycle_badge("experimental")
    usethis::use_pkgdown()
    usethis::use_github_action("pkgdown")
    usethis::use_github_labels(
      labels = geotidy:::.geotidy_labels(),
      colours = geotidy:::.geotidy_labels_brewer(),
      descriptions = geotidy:::.geotidy_labels_descriptions(),
      delete_default = TRUE
    )
  }
}
# nocov end
