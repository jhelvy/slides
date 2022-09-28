# First render the README
rmarkdown::render('README.Rmd', output_format = 'github_document')

# Then render the site
rmarkdown::render_site(encoding = 'UTF-8')
