# scrape colour names from https://www.htmlcsscolor.com/ and https://encycolorpedia.com/

require(rvest)
require(dplyr)

fields <- c("rgb", "name1", "source1", "name2", "source2", "date-accessed")

scrape_colour1 <- function(myurl_) {
	webpage <- read_html(myurl_)
	txt <- webpage %>% html_nodes(xpath="//h1[@id='uscBootStrapHeader_lblTitle']//text()")
	if (length(txt) == 3)
		return(as.character(txt[3]))
	return(NA)
}

scrape_colour2 <- function(myurl_) {
	webpage <- read_html(myurl_)
	title <- webpage %>% html_nodes(xpath="//section[@id='information']/h1/text()")
	title <- sub(" / *#.*", "", title)
	if (regexpr("^#", title)[1] < 0)
		return(title)
	txt <- webpage %>% html_nodes(xpath="//section[@id='information']/p//text()")
	txt <- gsub("  ", " ", sub("^ ", "", sub("shade of", "", sub("^ ", "", sub("^.*#[0-9a-fA-F]* is a", "", sub("\\..*", "", paste0(txt, collapse="")))))))
	return(txt)
}

scrape_colours <- function(colour_filename_) {
	# colour_filename_ = csv file to which results are appended
	if (file.exists(colour_filename_)) {
		master <<- file(colour_filename_, "at")
	} else {
		master <<- file(colour_filename_, "wt")
		writeLines(paste(fields, collapse=","), master) # write headers
	}
	#scrape <- tryCatch({
		# loop over colours
		for (r in 0:0xF * 0x110000) {
			for (g in 0:0xF * 0x1100) {
				 for (b in 0:0xF * 0x11) {
				 	hex <- sprintf("%06X", r + g + b)
				 	print(hex)
					myurl1 <- paste0("https://www.htmlcsscolor.com/hex/", hex)
					myurl2 <- paste0("https://encycolorpedia.com/", hex)
					tryCatch(
						{
							return_value1 <- scrape_colour1(myurl1)
						}, error = function(e) {
							message(paste0("Error on page ", myurl1, ":"))
							message(e)
						})
					tryCatch(
						{
							return_value2 <- scrape_colour2(myurl2)
						}, error = function(e) {
							message(paste0("Error on page ", myurl2, ":"))
							message(e)
						})
					if (is.na(return_value1) || class(return_value1) != "character")
						warning(paste("Could not find colour name on page ", myurl1))
					else if (is.na(return_value2))
						warning(paste("Could not find colour name on page ", myurl2))
					else {
						result <- paste(
							sapply(c(hex, return_value1, myurl1, return_value2, myurl2, date()), function(x) paste0("\"", x, "\"")),
							collapse=",")
						print(result)
						writeLines(result, master)
						}}}}
	#}, error = function(e) e)
	close(master)
}

scrape_colours("colours.csv")
