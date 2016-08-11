#' pivotviewR
#'
#' Create a new pivotviewer object from a data.frame, which should include the following columns:
#' \itemize{
#' \item Name: The name of the item
#' \item Description: a longer text description the displays when zoomed in on an item
#' \item Img: the filename of image, should be relative to ImgBase
#' \item Href: a webpage you want to link the item to
#' \item Id: optional unique item identifier
#' }
#' @param df the dataframe, should have these columns [Name, Description, Img, Href, Id]
#' @param CollectionName what your collection is called
#' @param ImgBase where your image files are located
#' @param colFormats format strings for the data, [n, p, m] = [number, percentage, money] followed by the number of decimal places.. i.e. "n2" means number with two decimal places
#' @param filterVisible list of true/false (for each column in df)
#' @param metadataVisible list of true/false (for each column in df)
#' @param searchable list of true/false (for each column in df)
#' @param width the width
#' @param height the height
#'
#' @import htmlwidgets
#'
#' @export
pivotviewR <- function(df, CollectionName, ImgBase = "", colFormats = NULL, filterVisible = NULL, metadataVisible = NULL, searchable = NULL, width = NULL, height = NULL) {

  # helper functions for types
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  is.POSIXlt <- function(x) inherits(x, "POSIXlt")
  is.Date <- function(x) inherits(x, "Date")

  # # convert factors to strings
   factorCols <- sapply(df, is.factor)
   df[factorCols] <- lapply(df[factorCols], as.character)
   # convert posixlt to posixct
   posixltCols <- sapply(df, is.POSIXlt)
   df[posixltCols] <- lapply(df[posixltCols], as.POSIXct)

   # Set up column formats
   if (is.null(colFormats)) {
     colFormats = rep('', ncol(df))
     colFormats[sapply(df, is.numeric)] <- 'n2'
   } else if (length(colFormats) != ncol(df)) {
     colFormats <- rep('', ncol(df))
     colFormats[sapply(df, is.numeric)] <- 'n2'
     print("The number of formats does not match the number of columns")
   }

   if(length(which(colnames(df) %in% c("Id","Name","Href","Img","Description"))) > 0) {
     colFormats <- colFormats[-which(colnames(df) %in% c("Id","Name","Href","Img","Description"))]
   }

   # Set up filters
   if (is.null(filterVisible)) {
     filterVisible <- rep('true', ncol(df))
   } else if (length(filterVisible) != ncol(df)) {
     filterVisible <- rep('true', ncol(df))
     print("The number of filterVisible does not match the number of columns")
   }

   if(length(which(colnames(df) %in% c("Id","Name","Href","Img","Description"))) > 0) {
     filterVisible <- filterVisible[-which(colnames(df) %in% c("Id","Name","Href","Img","Description"))]
   }

   # Set up metadata
   if (is.null(metadataVisible)) {
     metadataVisible <- rep('true', ncol(df))
   } else if (length(metadataVisible) != ncol(df)) {
     metadataVisible <- rep('true', ncol(df))
     print("The number of metadataVisible does not match the number of columns")
   }

   if(length(which(colnames(df) %in% c("Id","Name","Href","Img","Description"))) > 0) {
     metadataVisible <- metadataVisible[-which(colnames(df) %in% c("Id","Name","Href","Img","Description"))]
   }

   # Set up searchable
   if (is.null(searchable) ) {
     searchable <- rep('true', ncol(df))
   } else if (length(searchable) != ncol(df)) {
     searchable <- rep('true', ncol(df))
     print("The number of searchable does not match the number of columns")
   }

   if(length(which(colnames(df) %in% c("Id","Name","Href","Img","Description"))) > 0) {
     searchable <- searchable[-which(colnames(df) %in% c("Id","Name","Href","Img","Description"))]
   }

   if(!"Id" %in% colnames(df))
   {
     df$Id <- rownames(df)
   }

   if(!"Description" %in% colnames(df))
   {
     df$Description <- rep("",nrow(df))
   }

   if(!"Href" %in% colnames(df))
   {
     df$Href <- rep("#",nrow(df))
   }

   if(!"Name" %in% colnames(df))
   {
     df$Name <- rep("Unnamed",nrow(df))
   }

   if(!"Img" %in% colnames(df))
   {
     print("No Img Column! Please include a column named 'Img' that links to your images")
     df$Img <- rep("",nrow(df))
   }

  facets <- subset(df, select=-c(Id,Name,Href,Img,Description))

  colTypes <- rep('String', ncol(facets))
  colTypes[sapply(facets, is.numeric)] <- 'Number'
  colTypes[sapply(facets, is.Date)] <- 'DateTime'
  colTypes[sapply(facets, is.POSIXct)] <- 'DateTime'

  FacetCategories <- data.frame(Name = colnames(facets),
                                Type = colTypes,
                                Format = colFormats,
                                IsFilterVisible = filterVisible,
                                IsMetadataVisible = metadataVisible,
                                IsWordWheelVisible = searchable)

  x <- list(CollectionName = CollectionName, FacetCategories = FacetCategories, Items = df, ImgBase = ImgBase)

  # create widget
  htmlwidgets::createWidget(
    name = 'pivotviewR',
    x,
    width = width,
    height = height,
    package = 'pivotviewR'
  )
}

#' Shiny bindings for pivotviewR
#'
#' Output and render functions for using pivotviewR within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a pivotviewR
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name pivotviewR-shiny
#'
#' @export
pivotviewROutput <- function(outputId, width = 'auto', height = 'auto'){
  htmlwidgets::shinyWidgetOutput(outputId, 'pivotviewR', width, height, package = 'pivotviewR')
}

#' @rdname pivotviewR-shiny
#' @export
renderPivotviewR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, pivotviewROutput, env, quoted = TRUE)
}

