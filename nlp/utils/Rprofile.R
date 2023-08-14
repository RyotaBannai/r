#
if (file.exists("~/.Rprofile")) {
  oldRprofile <- paste("~/dot.Rprofile.",
    format(Sys.time(), "%Y_%m-%d_%H-%M"),
    ".txt",
    sep = ""
  )
  file.copy(from = "~/.Rprofile", to = oldRprofile)
}

download.file("http://rmecab.jp/R/dot.Rprofile.txt", dest = "~/.Rprofile")

source("~/.Rprofile")
