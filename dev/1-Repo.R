

usethis::use_git_ignore('notebook-ff')
usethis::use_git_ignore('*.Rproj')
usethis::use_git_ignore('*.Rproj.user')
usethis::use_build_ignore('notebook-ff')
usethis::use_build_ignore('README.md')
usethis::use_build_ignore('README.rmd')
usethis::use_testthat()

usethis::use_test('auditlogFlows')
usethis::use_test('fakeLog')

usethis::use_readme_rmd()
