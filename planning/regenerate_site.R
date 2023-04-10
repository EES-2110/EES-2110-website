library(rprojroot)
library(tidyverse)
pacman::p_load_gh("jonathan-g/blogdownDigest")
pacman::p_load_gh("jonathan-g/semestr")
# library(git2r)

regenerate_site <- function(root = NULL, force = FALSE, keep_tex = FALSE) {
  options(blogdown.method="html")

  if (is.null(root)) {
    root = find_root(criterion = has_file(".semestr.here"))
  }
  oldwd = setwd(root)
  on.exit(setwd(oldwd))
  message("Setting working directory to ", getwd())
  semester <- load_semester_db("planning/EES-2110.sqlite3")
  generate_assignments(semester)
  new_update_site(root = getwd(), force = force, keep_tex = keep_tex)
}

new_update_site <- function(root = NULL, force = FALSE, keep_tex = FALSE) {
  if (is.null(root)) {
    root = find_root(criterion = has_file(".semestr.here"))
  }
  oldwd = setwd(root)
  on.exit(setwd(oldwd))
  message("Setting working directory to ", getwd())
  update_site(force = force)
  out_opts = list(md_extensions = semestr:::get_md_extensions(), toc = TRUE,
                  includes = list(in_header = "ees2110.sty"),
                  keep_tex = keep_tex)
  update_pdfs(force_dest = TRUE, force = force, output_options = out_opts)
}

find_local_rmd_files <- function(dir) {
  list.files(dir, pattern = "[^_].*\\.Rmd", full.names = TRUE) %>%
    normalizePath()
}

update_extra_files <- function(root = NULL, dirs = NULL, force = FALSE,
                               keep_tex = FALSE, quiet = TRUE,
                               static_path = "static", content_path = "content") {
  if (is.null(root)) {
    root = find_root(criterion = has_file(".semestr.here"))
  }
  oldwd = setwd(root)
  on.exit(setwd(oldwd))
  message("Setting working directory to ", getwd())

  cd <-  paste0(normalizePath(getwd(), winslash = "/"), "/")
  dir <- normalizePath(dir, winslash = "/")
  dir <- stringr::str_replace(dir, stringr::fixed(cd), "")

  if (! is.null(dirs)) {
    dirs <- map_chr(dirs, function(d) {
      if (! dir.exists(d)) {
        d <- file.path(content_path, d)
        if (! dir.exists(d)) {
          d <- NULL
        }
      }
      d
    })
    files <- map_chr(dirs, find_local_rmd_files)
    if (force) {
      to_build <- files
    } else {
      to_build <- semestr:::pdfs_to_rebuild(files, root, static_path, content_path)
    }
    to_build <- normalizePath(to_build, winslash = "/") %>%
      str_replace(fixed(cd), "")

    if (! quiet) {
      message("Building ", length(to_build), " out of date ",
              ifelse(length(to_build) == 1, "file", "files"),
              "; candidate list has ", length(files), " ",
              ifelse(length(files) == 1, "file", "files"),
              " in total.")
    }

    for (f in to_build) {
      semestr:::build_pdf_from_rmd(f, root, static_path, force_dest = force_dest,
                         output_options = output_options)
      semestr:::update_pdf_file_digests(f, root_dir, static_path, content_path,
                              partial = TRUE)
    }

    # message("On exit stack: ", deparse(sys.on.exit()))
    invisible(to_build)
    }
  }


init_git_tokens <- function(keyring = "git_access") {
  if (keyring::keyring_is_locked(keyring)) {
    try(keyring::keyring_unlock(keyring), silent = TRUE)
    if (keyring::keyring_is_locked(keyring)) {
      warning("Could not unlock keyring.")
      return(invisible(NULL))
    }
  }
  keys <- keyring::key_list(keyring = keyring)
  gh_pat <- keys %>% filter(service == "GITHUB_PAT")
  gl_pat <- keys %>% filter(service == "GITLAB_PAT")
  Sys.setenv(GITHUB_PAT = keyring::key_get(gh_pat$service,
                                           username = gh_pat$username,
                                           keyring = keyring))
  Sys.setenv(GITLAB_PAT = keyring::key_get(gl_pat$service,
                                           username = gl_pat$username,
                                           keyring = keyring))
}

config_cred <- function(val, lbl, repo) {
  if(val) {
    url <- git2r::remote_url(repo, lbl)
    key_path <- NULL
    # message("url = ", url)
    if (str_starts(url, fixed("git@github.com"))) {
      key_path <- git2r::ssh_path(file.path("github.com",
                                            "id_ed25519_gh"))
    } else if (str_starts(url, fixed("git@gitlab.com"))) {
      key_path <- git2r::ssh_path(file.path("gitlab.com",
                                            "id_ed25519_gl_com"))
    } else if (str_starts(url, fixed("git@gitlab.jgilligan.org"))) {
      key_path <- git2r::ssh_path(file.path("jg_gitlab", "id_ed25519"))
    }
    if (! is.null(key_path)) {
      # message("key_path = ", key_path)
      git2r::cred_ssh_key(
        publickey = str_c(key_path, ".pub"),
        privatekey = key_path,
        passphrase = keyring::key_get("SSH_KEY_PASSWORD",
                                      keyring = "git_access",
                                      username = "jonathan")
      )
    } else {
      NULL
    }
  } else {
    git2r::cred_token(ifelse(lbl == "origin", "GITLAB_PAT",
                             "GITHUB_PAT"))
  }
}

publish <- function(ssh = NULL, repo = ".") {
  init_git_tokens()
  repo = git2r::repository(repo)
  remotes <- c("origin", "publish")

  if (is.null(ssh)) {
    pattern <- "^git@([a-zA-Z][a-zA-Z0-9_\\-.]+):"
    ssh <- map_lgl(remotes, ~str_detect(git2r::remote_url(repo, .x),
                                        pattern)) %>%
      set_names(remotes)
  }

  if (length(ssh) < 2) {
    ssh <- rep_len(ssh, 2) %>% set_names(remotes)
  }

  cred <- imap(ssh, config_cred, repo = repo)
  git2r::push(".", name = "publish", refspec = "refs/heads/main",
              credentials = cred$publish)
  git2r::push(".", name = "origin", refspec = "refs/heads/main",
              credentials = cred$origin)
}
