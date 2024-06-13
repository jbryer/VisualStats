out_dir <- '_site/shiny/'
base_dir <- 'inst/shiny/'
apps <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)

quarto::quarto_render('book/')

for(i in apps) {
	shinylive::export(appdir = paste0(base_dir, i),
					  destdir = out_dir,
					  subdir = i)
}

if(FALSE) { # For local testing
	httpuv::runStaticServer("_site/", port = 2112, background = TRUE, browse = FALSE)
	browseURL(paste0('http://localhost:2112/shiny/', apps[1]))
	httpuv::stopAllServers()
}
