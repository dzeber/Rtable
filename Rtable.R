#######################################################################
###
###  Generate a nice HTML table from a data frame or data table. 
###  HTML display uses jQuery and Dynatable. 
###
#######################################################################


table.to.HTML = local({

## Preset file system for HTML.
##
.filesys = list(dir = list(css = "css", js = "js"),
                js = c("jquery.dynatable.min.js", "jquery-1.7.2.min.js",
                    "rtable.js"),
                css = c("jquery.dynatable.css", "rtable.css"),
                template = "tabletemplate.html")
    
## Formatters to apply from rtable.js. 
##
.formatters = list(
    count = list(type = "contentformat", name = "countFormat"),
    singleprop = list(type = "cellwriter", name = "propBar"),
    multiprop = list(type = "cellwriter", name = "multiBar")
)
                
    
## Want to store the path to the script file 
## (which is the place to find the HTML files). 

## Get the directory from which the script was called. 
.script.dir = dirname(sys.frame(1)$ofile)

## We want this to be an absolute path. 
## Hack: switch to the js folder, and check if we can access the HTML template 
## (in the base dir). This should fail if path is relative, unless relative path
## contains "..". 
## If path appears to be relative, throw an error. 
if(grepl("..", .script.dir, fixed = TRUE)) 
    stop("The Rtable needs to be sourced from an absolute path")

currwd = setwd(file.path(.script.dir, .filesys$dir$js))
bad.path = !file.exists(file.path(.script.dir, .filesys$template)) 
setwd(currwd)
if(bad.path) stop("The Rtable needs to be sourced from an absolute path")
rm(currwd, bad.path)


## Create directory structure for HTML page files. 
## Specify the directory to work in. 
##
## Returns the path to the base dir. 
##
create.dir.structure = function(dest.path, .params) {
    ## First try switching to destination directory. 
    currwd = tryCatch(setwd(dest.path), error = function(e) {
        stop("Unable to access specified destination path.")
    })
    
    ## Create directory structure. 
    try.creating.dir(.params$basedir)
    lapply(file.path(.params$basedir, .filesys$dir), try.creating.dir)
    try.creating.dir(file.path(.params$basedir, .params$datadir))
    
    ## Copy in necessary files (JS and CSS). 
    for(d in c("js", "css")) {
        file.copy(from = file.path(.script.dir, .filesys$dir[[d]], .filesys[[d]]), 
                    to = file.path(.params$basedir, .filesys$dir[[d]]), 
                    overwrite = FALSE)
    }
    setwd(currwd)
    file.path(dest.path, .params$basedir)
}

## Try creating directory. 
##
## Returns TRUE if creation succeeds or FALSE if directory already exists.
## Throws an error if creation fails. 
##
try.creating.dir = function(new.dir) {
    if(file.exists(new.dir)) return(FALSE)
    if(!dir.create(new.dir))
        stop(sprintf("Unable to create directory %s", new.dir))
    TRUE
}

## Convert data frame or data table to JSON array of objects representing rows.
## Supply the data frame to convert and the path to the HTML base dir. 
##
## Returns the path to the data file relative to the base directory. 
##
make.json = function(dd, base.path, .params) {
    require("rjson")
    ## Create the path to the data file relative to the base directory. 
    data.path = file.path(.params$datadir, paste0(.params$datafile, ".json"))
    jsonList = list(data = lapply(1:nrow(dd), function(i) { 
        ## If one of the columns was specified as a list, element for that row
        ## will be a list of length 1 - need to unlist one level. 
        lapply(as.list(dd[i,]), function(r) {
            if(is.list(r)) unlist(r, recursive = FALSE) else r
        })
    }))
    sink(file.path(base.path, data.path))
    cat(toJSON(jsonList))
    sink()
    data.path
}

## Populate the HTML template. 
## Supply the path to the HTML base dir, the HTML page title,
## the heading to display above the table, the path to the data file 
## relative to the base HTML directory, the contents of the HTML header block, 
## and the top and bottom HTML portions. 
##
## Returns the path to the HTML file. 
##
make.html = function(base.path, 
                    page.title,
                    heading,
                    data.path,
                    header.block,
                    top.html,
                    bottom.html,
                    .params) {
    html.path = file.path(base.path, paste0(.params$htmlfile, ".html"))
    html = paste(readLines(file.path(.script.dir, .filesys$template)), collapse = "\n")
    html = sprintf(html, page.title, top.html, heading, data.path, 
                    header.block, bottom.html)
    sink(html.path)
    cat(html)
    sink()
    html.path
}

## Generates HTML table header row from the column information list. 
##
## Returns a string of HTML representing the header row. 
##
make.header.row = function(column.info) {
    paste0(c("<tr>", unlist(lapply(names(column.info), function(hn) {
        ci = column.info[[hn]]
        style = ci[c("width", "max-width", "min-width", "style")]
        nullstyle = sapply(style, is.null)
        style = if(!all(nullstyle)) {
            style = style[!nullstyle]
            widths = names(style)[names(style) != "style"]
            widths = if(length(widths) > 0) { 
                paste(unlist(lapply(widths, function(sn) { 
                    sprintf("%s: %s;", sn, style[[sn]]) 
                })), collapse = "")
            } else { "" }
            sprintf(" style='%s%s'", widths, 
                if(is.null(style$style)) "" else style$style)
        } else { "" }
        fmt = if(!is.null(ci$formatter)) {
            sprintf(" data-%s='%s'", 
                ci$formatter$type,
                ci$formatter$name)
        } else { "" }
        cl = if(!is.null(ci$class)) {
            sprintf(" class='%s'", paste(ci$class, collapse = " "))
        } else { "" }
        sprintf("<th data-dynatable-column='%s'%s%s%s>%s</th>", hn, 
            cl, style, fmt, ci$label)
    })), "</tr>"), collapse = "\n")
}

## Generates extra HTML table header row to prepend above main row. 
## Input is list of lists containing elements 'ncols' and optionally 'label'.
## For each sublist, a header cell will be created that extends for ncols columns
## with the specified header (empty if header is NULL).
## If ncols is NUll, it will be assumed to be 1.
##
## Returns a string of HTML representing the header row. 
##
make.outer.header.row = function(outer.headers) {
    paste0(c("<tr>", unlist(lapply(outer.headers, function(hh) {
        sprintf("<th%s>%s</th>", 
            if(!is.null(hh$ncol) && hh$ncol > 1) {
                sprintf(" colspan='%s'", hh$ncol)
            } else { "" },
            if(!is.null(hh$label)) { hh$label } else { "" })
    })), "</tr>"), collapse = "\n")
}


## Creates HTML table display from data frame. 
## Creates HTML file system as necessary, generates JSON version of data frame, 
## and generates HTML file load display. 
##
## Arguments: 
##  dataset - the data frame
##  destination.path - the path to the destination directory in which 
##                      to create the HTML files
##  row.index - whether or not to include row indices as a column on the left. 
##
##  heading - the heading to display above the table
##  column.info - a list of information tagged by column names 
##      for columns to display. Ordering in the list determines ordering 
##      in the table. If column is missing, it will not be displayed. 
##      If column.info is NULL, entire data frame will be displayed. 
##      Each element should be a list with any of the follow elements: 
##      - label - the column heading to display
##      - formatter - the formatter to apply - must be one of 
##          "count", "singleprop", "multiprop"
##      - class - a vector of CSS classes to apply
##      CSS:
##      - width - the desired width of the column, including units
##      - maxwidth - the maximum width, including units
##      - minwidth - the maximum width, including units
##      - style - generic CSS
##  outer.headings - specification for additional header rows. 
##      A list with an element for each additional row. Each element is a list 
##      of lists containing elements 'ncol' and optionally 'label', 
##      representing a cell spanning 'ncol' columns with given label.
##  page.title - the HTML page title
##
##  html.filename - the name for the HTML file (without extension)
##  base.dirname - the name of the base directory for the HTML files 
##                  (gets created as a subdir of destination.path)
##  data.dirname - the name of the directory to contain the data file
##                 (a subdir of the base dir)
##  data.filename - the name for the JSON file (without extension)
## 
##  top.html - optional extra HTML to add above the table
##  bottom.html - optional extra HTML to add below the table
##
##  open.in.browser - whether or not to open HTML file once it has been created
##
## Returns the path to the HTML file. 
##
function(dataset,
        destination.path,
        row.index = FALSE,
        heading = NULL,
        column.info = NULL, 
        outer.headings = NULL,
        page.title = heading,
        html.filename = "table",
        base.dirname = "html",
        data.dirname = "data",
        data.filename = "tabledata",
        top.html = NULL,
        bottom.html = NULL,
        open.in.browser = TRUE) {
    ## Validate inputs and set parameters. 
    if(is.null(dataset) || !is.data.frame(dataset))
        stop("'dataset' must be a non-null data frame")
    
    if(!is.character(destination.path) || length(destination.path) != 1)
        stop("'destination.path' must a character vector of length 1")
    
    if(is.null(heading)) heading = ""
    if(is.null(page.title)) page.title = ""
    if(is.null(top.html)) top.html = ""
    if(is.null(bottom.html)) bottom.html = ""
    
    ## Check column info parameters. 
    if(is.null(column.info)) {
        ## If column info is null, display all columns in data frame. 
        column.info = setNames(lapply(names(dataset), function(nn) {
            list(label = nn)
        }), names(dataset))
    } else {
    # else {
        # other.names = names(data)[!(names(data) %in% names(column.info))]
        # column.info = c(other.names, setNames(nm = other.names))
    # }
        ## If label is missing from any element of column.info, use column name. 
        for(cn in names(column.info)) {
            if(is.null(column.info[[cn]]$label))
                column.info[[cn]]$label = cn
        }
        ## Check that formatters are recognized. 
        fmts = unlist(lapply(column.info, "[[", "formatter"))
        if(length(fmts) > 0) {
            fmts = setNames(charmatch(fmts, names(.formatters)), names(fmts))
            bad.fmt = is.na(fmts) | fmts == 0
            if(any(bad.fmt))
                stop(sprintf("Formatters for column%s '%s' in 'column.info' were not recognized", 
                    if(sum(bad.fmt) > 1) "s" else "",
                    paste(names(bad.fmt)[bad.fmt], collapse = "','")))
            for(cn in names(fmts)) {
                column.info[[cn]]$formatter = .formatters[[fmts[[cn]]]]
            }
        }
    }
    
    ## Check outer headings. 
    if(!is.null(outer.headings)) {
       if(any(c("ncol", "label") %in% names(outer.headings[[1]]))) {
            ## Assume we want a single row. 
            ## Wrap in a list to conform to the desired structure. 
            outer.headings = list(outer.headings)
        }
        
        ## Check that column lengths add up. 
        bad.ncols = unlist(lapply(outer.headings, function(hr) {
            sum(unlist(lapply(hr, function(hh) {
                if(is.null(hh$ncol)) 1 else hh$ncol
            })))})) != length(column.info)
        if(any(bad.ncols)) {
            stop(sprintf("The column numbers in header row%s %s don't add up to the number of data columns", 
                if(sum(bad.ncols) > 1) "s" else "",
                paste(which(bad.ncols), collapse = ",")))
        }
        
        ## Add column for row index if necessary. 
        if(row.index) {
            outer.headings = lapply(outer.headings, function(r) {
                c(list(ncol = 1), r)
            })
        }
    }
    
    if(row.index) {
        column.info = c(row.index = list(NULL), column.info)
        column.info$row.index$label = ""
        column.info$row.index$class = "rowindex"
        dataset$row.index = 1:nrow(dataset)
    }
    
    for(aa in c("base.dirname", "data.dirname", 
                "html.filename", "data.filename")) {
        if(!is.character(get(aa)) || length(get(aa)) != 1)
            stop(sprintf("'%s' must be a character vector of length 1", aa))
    }
    
    params = list()
    params[["basedir"]] = base.dirname
    params[["datadir"]] = data.dirname
    params[["htmlfile"]] = html.filename
    params[["datafile"]] = data.filename
    
    ## Create directory structure. 
    base.path = create.dir.structure(destination.path, params)
    
    ## Create JSON. 
    data.rel.path = make.json(dataset, base.path, params)
    
    ## Generate the HTML file. 
    ## First create header block. 
    header.block = make.header.row(column.info)
    if(!is.null(outer.headings)) {
        header.block = paste(c(
            unlist(lapply(outer.headings, make.outer.header.row)),
            header.block), collapse = "\n")
    }
    
   html.path = make.html(base.path, page.title, heading, data.rel.path, 
                    header.block, top.html, bottom.html, params)
    
    ## If required, open html file in browser. 
    if(open.in.browser) {
        browseURL(paste0("file://", html.path))
    }
    
    ## Return path to HTML file. 
    html.path
}

})

