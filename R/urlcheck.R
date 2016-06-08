list.repos <- function() {

  me <- github::get.myself()$content$login

  repos.list <- github::get.my.repositories()
  repos.names <- unlist(lapply(repos.list$content, "[[", "name"))

  return(list(login=me, names=repos.names))

}

scan.file <- function(repo, file) {

  message(paste0("Scanning for ",repo))

  repo.details <- list.repos()

  if(!repo %in% repo.details[["names"]]) stop(paste0(repo," is not a known repository of yours."))
  message(paste0("Found repository: ",repo,", scanning ", file))

  # readme <- get.repository.readme(owner=repo.details[["login"]], repo=repo, param="raw")
  readme <- get.repository.path(owner=repo.details[["login"]], repo=repo, path=file)

  readme_html <- xml2::read_html(rawToChar(base64enc::base64decode(readme$content$content)))

  ## html processing
  # anchors <- rvest::html_nodes(readme_html, "a")
  # hrefs <- unlist(lapply(rvest::html_attrs(anchors), "[[", "href"))

    ## md processing
  text <- rvest::html_text(readme_html)
  hrefs <- stringr::str_extract_all(text, 'http[^\\)]*', simplify=TRUE)

  message(paste0("Found ",length(hrefs)," URLs."))
  # print(hrefs)

  obj <- list(login=repo.details[["login"]],
              hrefs=hrefs)

  return(obj)

}

test.urls <- function(repo, file) {

  invisible(githuburlcheckr_setup())

  repo.details <- scan.file(repo, file)

  test.result <- data.frame(href=c(repo.details$hrefs), stringsAsFactors=FALSE)
  test.result$code <- NA_integer_

  for(url in seq_along(repo.details$href)) {

    test.url <- try(httr::HEAD(test.result$href[url]), silent=TRUE)
    if (!inherits(test.url, "try-error")) {
      test.result$code[url] <- test.url$status_code
    } else {
      test.result$code[url] <- 0
    }

  }

  return(test.result)

}
