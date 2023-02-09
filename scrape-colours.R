# scrape colour names from https://www.htmlcsscolor.com/

require(rvest)
require(dplyr)

fields <- c("rgb", "name", "source", "date-accessed")

scrape_colour <- function(myurl_) {
	webpage <- read_html(myurl_)
	txt <- webpage %>% html_nodes(xpath="//h1[@id='uscBootStrapHeader_lblTitle']//text()")
	if (length(txt) == 3)
		return(as.character(txt[3]))
	return(NA)
}

scrape_colours <- function(colour_filename_) {
	# colour_filename_ = csv file to which results are appended
	if (file.exists(colour_filename_)) {
		master <- file(colour_filename_, "at")
	} else {
		master <- file(colour_filename_, "wt")
		writeLines(paste(fields, collapse=","), master) # write headers
	}
	#scrape <- tryCatch({
		# loop over colours
		for (r in 0:0xF * 0x110000) {
			for (g in 0:0xF * 0x1100) {
				 for (b in 0:0xF * 0x11) {
				 	hex <- sprintf("%06X", r + g + b)
				 	print(hex)
					myurl <- paste0("https://www.htmlcsscolor.com/hex/", hex)
					tryCatch(
						{
							return_value <- scrape_colour(myurl)
							if (is.na(return_value)) {
								warning(paste("Could not find colour name on page ", myurl))
							} else if (class(return_value) == "character") {
								result <- paste(
									sapply(c(hex, return_value, myurl, date()), function(x) paste0("\"", x, "\"")),
									collapse=",")
								print(result)
								writeLines(result, master) 
							}
						}, error = function(e) {
							message(paste0("Error on page ", myurl, ":"))
							message(e)
						})}}}
	#}, error = function(e) e)
	close(master)
}

scrape_colours("colours.csv")
