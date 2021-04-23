library(httr)
library(XML)
library(jsonlite)
library(rmarkdown)

dataset.url <- 'https://www.kaggle.com/epa/carbon-monoxide'

resp <- GET(dataset.url)
cont <- content(resp, as='text')
html <- htmlParse(cont, asText = TRUE)

# Get peace where Description appeares
scriptEl <- xpathSApply(html, '//script[@class="kaggle-component"]', xmlValue)[[2]]
# Split script by end of commands
commands <- strsplit(scriptEl, ';')[[1]]
# By viewing I found out that description is in 3-rd and 4-th lines of script.
# It was separated by mistake
needed.command <- paste(commands[[3]], commands[[4]], sep = ';')
cat(needed.command)

# Now extract exactly object to parse it afterward
json.str <- sub('Kaggle.State.push(', '', needed.command, fixed = TRUE)
json.str <- sub(')$', '', json.str)

# Parse this string as JSON
json <- parse_json(json.str)

# Extract description
description <- json$description
# Repair headers (no spacing between hash symbol and header title)
description <- gsub('###', '### ', description, fixed = TRUE)

# Write it into .md file
write(description, file = 'markdowns/epa_co_daily_description.md')
render(input = 'markdowns/epa_co_daily_description.md',
       output_format = 'pdf_document',
       output_dir = 'docs')

# Sys.time()
cat("\n\n",
    "Script was run at",
    "2021-04-23 12:17:43 EEST",
    "\n")


# Clearing environment variables
rm(dataset.url,
   resp, cont, html,
   scriptEl, commands, needed.command,
   json.str, json, description)
# Disconnecting packages
detach('package:httr', unload = TRUE)
detach('package:XML', unload = TRUE)
detach('package:jsonlite', unload = TRUE)
detach('package:rmarkdown', unload = TRUE)
# Done