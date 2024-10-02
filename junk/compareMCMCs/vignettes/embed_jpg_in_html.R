
embed_jpg_in_html <- function(htmlfile, jpgfile, overwrite = FALSE) {
  htmlorig <- readLines(htmlfile)
  htmlupdated <- if(overwrite) htmlfile else gsub(".html", "_modified.html", htmlfile)
  txtfile <- gsub(".jpg", ".txt", jpgfile)
  system2("base64", c("-i", jpgfile, "-o", txtfile))
  binarytxt <- readLines(txtfile)
  htmlmodified <- gsub(jpgfile, paste0("data:image/jpg;base64,", binarytxt), htmlorig)
  writeLines(htmlmodified, con = htmlupdated)
}
