#!/usr/bin/env Rscript
# dev/build_docs.R  --  THE canonical way to rebuild the menstrualcycleR pkgdown site.
#
# WHY THIS SCRIPT EXISTS
#   pkgdown's pkgdown:::package_mds() globs EVERY root-level *.md and renders it onto the
#   public site (only README/LICENSE/NEWS/cran-comments + issue/PR templates are whitelisted).
#   So a plain `pkgdown::build_site()` publishes CLAUDE.md AND the gitignored CLAUDE.local.md
#   as docs/CLAUDE.html / docs/CLAUDE.local.html, and copies their text into docs/search.json,
#   docs/llms.txt and docs/sitemap.xml. `.Rbuildignore` does NOT stop this (it only affects the
#   R CMD build tarball). The docs/ folder is what GitHub Pages serves, so any leak goes public.
#
#   This script makes the machine catch that failure instead of a person:
#     1. hides every root CLAUDE*.md for the duration of the build (restored even on error), then
#     2. rebuilds the whole site, then
#     3. scans docs/ for a LEAKED PAGE -- a file named claude*, or any file referencing one by
#        filename -- and STOPS WITH AN ERROR (non-zero exit) if it finds one, so a slipped leak
#        can never be silently committed/published. See check_docs_clean() for why the scan keys
#        on the leak's structure rather than on the word "claude" (narrowed 2026-09-29).
#
# USAGE  (run from the package root -- the MAIN checkout, not a worktree; CLAUDE.local.md
#         lives in the main checkout):
#     Rscript --vanilla dev/build_docs.R            # hide -> build_site -> leak-check
#     Rscript --vanilla dev/build_docs.R --selftest # prove the leak scanner works, no build
#
#   --vanilla is REQUIRED: it skips the renv .Rprofile so the build runs from the GLOBAL R
#   library, which has the full build toolchain (pkgdown, rmarkdown, knitr, pkgload,
#   marginaleffects). renv::restore() alone is insufficient -- the lockfile has runtime
#   Imports only. See memory `docs-site-build-and-publish`.
#
# COMPANION BACKSTOP: dev/git-hooks/pre-commit (install once per clone with
#   dev/install-hooks.sh) blocks committing any docs/ file that references "claude" even when
#   the site was rebuilt in RStudio without this wrapper. Belt and suspenders.

## ---- pandoc ---------------------------------------------------------------------------------
## pkgdown/rmarkdown need pandoc, which is not on PATH on the lab Macs. Honor an existing
## RSTUDIO_PANDOC; otherwise fall back to RStudio's bundled arm64 quarto pandoc. If neither is
## available, let rmarkdown find pandoc itself and warn.
default_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && dir.exists(default_pandoc)) {
  Sys.setenv(RSTUDIO_PANDOC = default_pandoc)
}

## ---- the guard: scan a rendered site for a CLAUDE*.md leak ----------------------------------
## WHAT COUNTS AS A LEAK (narrowed 2026-09-29 -- see the note below):
##   1. NAME -- any file under `docs_dir` whose own name contains "claude". This is the leak
##      itself: the rendered page is the thing GitHub Pages would publish.
##   2. REFERENCE -- any file whose text names a claude page ("claude.html", "claude.md",
##      "claude.local"). That is how a leaked page reaches the indexes, and each leg is verified
##      against real pkgdown output: sitemap.xml lists EVERY page by filename, search.json
##      carries a "path" per page that has prose, and llms.txt link-lists use the same paths.
## Binary files (PNGs etc.) are skipped via a NUL-byte test.
##
## DELIBERATELY NOT A LEAK: the bare word "claude" in a page's prose.
## Until 2026-09-29 this scan tripped on that word anywhere, with one hard-coded allowlisted
## sentence (the explainer's author byline). That rule stopped being usable once the package
## added its AI-tool disclosures: the README's "Use of AI coding tools" section, two sentences in
## the 1.0.0 changelog entry, and the preparing-your-data vignette footer all name Claude Code
## ON PURPOSE, and all three render into docs/. The word-scan therefore blocked EVERY site
## rebuild, on the package's own transparency statements. Allowlisting those sentences was the
## obvious repair and was rejected: pkgdown also writes stop-word-stripped copies of them into
## search.json ("vignette drafted Claude Code reviewed package authors"), so the allowlist would
## have had to carry mangled twins that change whenever pkgdown changes its search index. The two
## checks above key on the leak's STRUCTURE instead, so they neither false-positive on a
## deliberate disclosure nor depend on how pkgdown handles prose. The trade-off, stated plainly:
## internal text pasted by hand into an otherwise legitimate page is no longer caught here. That
## was never this guard's job -- it exists for the pkgdown root-*.md render leak.
check_docs_clean <- function(docs_dir = "docs") {
  if (!dir.exists(docs_dir)) {
    stop(sprintf("check_docs_clean(): '%s' not found -- run from the package root.", docs_dir))
  }
  ## A reference to a RENDERED claude page. Matched case-insensitively and literally, so there is
  ## no regex to mis-escape; "claude.local" covers CLAUDE.local.md/.html in either rendering.
  page_refs <- c("claude.html", "claude.md", "claude.local")
  files <- list.files(docs_dir, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE)
  hits <- character(0)
  for (f in files) {
    if (dir.exists(f)) next
    ## 1) the leaked page itself: docs/CLAUDE.html, docs/CLAUDE.local.html, ...
    if (grepl("claude", basename(f), ignore.case = TRUE)) {
      hits <- c(hits, f)
      next
    }
    ## 2) an index pointing at one: sitemap.xml / search.json / llms.txt
    sz <- file.info(f)$size
    if (is.na(sz) || sz == 0) next
    raw <- readBin(f, what = "raw", n = sz)
    if (any(raw == as.raw(0L))) next            # NUL byte => binary asset, skip
    txt <- tolower(tryCatch(rawToChar(raw), error = function(e) ""))
    if (any(vapply(page_refs, function(p) grepl(p, txt, fixed = TRUE), logical(1)))) {
      hits <- c(hits, f)
    }
  }
  if (length(hits)) {
    stop(sprintf(
      paste0("DOCS LEAK BLOCKED: %d file(s) under '%s' are, or point at, a rendered CLAUDE*.md ",
             "page:\n%s\nThe hide-during-build failed. Rebuild via ",
             "`Rscript --vanilla dev/build_docs.R` and re-check."),
      length(hits), docs_dir, paste0("  ", hits, collapse = "\n")))
  }
  message(sprintf("OK: no CLAUDE*.md page leak in '%s' (%d files scanned).",
                  docs_dir, length(files)))
  invisible(TRUE)
}

## ---- hide every root CLAUDE*.md for the duration of `expr`, then restore -------------------
## Guarantees restore even if `expr` errors (that's the whole safety point), via on.exit().
## `^CLAUDE.*\\.md$` matches CLAUDE.md, CLAUDE.local.md, and any future CLAUDE*.md. (In a .R
## FILE the `\\.` is correct; the shell mangles it only in the `Rscript -e` form.) list.files()
## default all.files=FALSE skips the `.<name>.bak` dotfiles, so the backups are never re-globbed.
with_claude_md_hidden <- function(expr) {
  mds <- list.files(".", pattern = "^CLAUDE.*\\.md$", ignore.case = TRUE)
  if (length(mds)) {
    baks <- paste0(".", mds, ".bak")
    file.rename(mds, baks)
    on.exit(file.rename(baks, mds), add = TRUE)   # restore even if expr errors
    message(sprintf("Hid %d CLAUDE*.md during build: %s",
                    length(mds), paste(mds, collapse = ", ")))
  } else {
    message("No root CLAUDE*.md found to hide (nothing to leak from this checkout).")
  }
  force(expr)
}

## ---- the build: hide CLAUDE*.md, rebuild the whole site, then guard -------------------------
build_docs <- function() {
  if (!file.exists("DESCRIPTION")) {
    stop("Run from the package root (no DESCRIPTION in the working directory).")
  }
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("pkgdown is not installed in this library. Run with `Rscript --vanilla` so the ",
         "build uses the global library, not renv.")
  }
  with_claude_md_hidden(pkgdown::build_site(pkg = ".", preview = FALSE))
  check_docs_clean("docs")   # the machine catches the leak here, before anyone can commit it
  message("Docs rebuilt and leak-checked clean.")
  invisible(TRUE)
}

## ---- self-test: prove the guard blocks a simulated leak, without a 30s build ----------------
selftest <- function() {
  ## (a) scanner must ERROR on a planted leak: the page itself, and each index that points at it
  ##     (fixtures copy the shapes real pkgdown output uses -- see check_docs_clean()).
  leak <- file.path(tempdir(), "docs_leaktest")
  unlink(leak, recursive = TRUE); dir.create(leak)
  on.exit(unlink(leak, recursive = TRUE), add = TRUE)
  writeLines("<html>clean page</html>",        file.path(leak, "index.html"))
  writeLines("<html>rendered CLAUDE.md</html>", file.path(leak, "CLAUDE.html"))      # the page
  writeLines("<urlset><loc>https://x/CLAUDE.html</loc></urlset>",
             file.path(leak, "sitemap.xml"))                                          # sitemap ref
  writeLines("[{\"path\": \"https://x/CLAUDE.html\", \"text\": \"notes\"}]",
             file.path(leak, "search.json"))                                          # search ref
  caught <- tryCatch({ check_docs_clean(leak); FALSE },
                     error = function(e) { message("  scanner reported: ", conditionMessage(e)); TRUE })
  if (!caught) stop("SELFTEST FAILED: scanner did NOT catch a planted leak.")
  ## each of the three must trip on its own, so no one check is carrying the others
  for (one in c("CLAUDE.html", "sitemap.xml", "search.json")) {
    solo <- file.path(tempdir(), "docs_solotest")
    unlink(solo, recursive = TRUE); dir.create(solo)
    file.copy(file.path(leak, one), file.path(solo, one))
    if (!tryCatch({ check_docs_clean(solo); FALSE }, error = function(e) TRUE))
      stop(sprintf("SELFTEST FAILED: '%s' alone did not trip the scanner.", one))
    unlink(solo, recursive = TRUE)
  }

  ## (b) scanner must PASS on a clean site
  clean <- file.path(tempdir(), "docs_cleantest")
  unlink(clean, recursive = TRUE); dir.create(clean)
  on.exit(unlink(clean, recursive = TRUE), add = TRUE)
  writeLines("<html>all good, nothing to see</html>", file.path(clean, "index.html"))
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x00, 0x01)), file.path(clean, "logo.png")) # binary w/ NUL
  ok <- tryCatch({ check_docs_clean(clean); TRUE }, error = function(e) FALSE)
  if (!ok) stop("SELFTEST FAILED: scanner flagged a clean site.")

  ## (b2) REGRESSION, 2026-09-29: the package's own AI-tool disclosures must NOT trip the
  ## scanner. Each string below is copied from the real rendered site -- the README section, the
  ## 1.0.0 changelog, the vignette footer, the explainer byline that used to need a hard-coded
  ## allowlist, and the stop-word-stripped shape pkgdown writes into search.json. Before the
  ## 2026-09-29 narrowing, every one of these blocked the build.
  disc <- file.path(tempdir(), "docs_disclosuretest")
  unlink(disc, recursive = TRUE); dir.create(disc)
  on.exit(unlink(disc, recursive = TRUE), add = TRUE)
  writeLines(c("<p>and Claude Code, used from June 2026 onward and carrying a",
               "<code>Co-Authored-By</code> trailer.</p>"), file.path(disc, "index.html"))
  writeLines(c("<p>Claude Code was used for this release: the CRAN packaging changes above,",
               "the new vignette, and 49 carry a Claude Code Co-Authored-By trailer.</p>"),
             file.path(disc, "news.html"))
  writeLines("<p>This vignette was drafted with Claude Code and reviewed by the authors.</p>",
             file.path(disc, "article.html"))
  writeLines("<p>Created by Tory Eisenlohr-Moul, PhD with Claude Code</p>",
             file.path(disc, "pacts-explainer.html"))
  writeLines("[{\"text\": \"vignette drafted Claude Code reviewed package authors\"}]",
             file.path(disc, "search.json"))
  ok2 <- tryCatch({ check_docs_clean(disc); TRUE },
                  error = function(e) { message("  ", conditionMessage(e)); FALSE })
  if (!ok2) stop("SELFTEST FAILED: scanner flagged the package's deliberate AI-tool disclosures.")

  ## (c) with_claude_md_hidden() must restore CLAUDE*.md EVEN WHEN the build throws -- the
  ## whole safety guarantee. Run it in a throwaway dir so the real CLAUDE.md is untouched.
  sandbox <- file.path(tempdir(), "hide_restore_test")
  unlink(sandbox, recursive = TRUE); dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(sandbox)
  writeLines("# fake CLAUDE.md",       "CLAUDE.md")
  writeLines("# fake CLAUDE.local.md", "CLAUDE.local.md")
  probe <- new.env()   # reference-semantics capture (avoids `<<-` scoping pitfalls)
  tryCatch(
    with_claude_md_hidden({
      probe$hidden_during <- !file.exists("CLAUDE.md") && !file.exists("CLAUDE.local.md")
      stop("simulated build failure")           # force the error path
    }),
    error = function(e) "errored as expected")
  setwd(old_wd)
  if (!isTRUE(probe$hidden_during)) stop("SELFTEST FAILED: CLAUDE*.md were not hidden during the build.")
  if (!all(file.exists(file.path(sandbox, c("CLAUDE.md", "CLAUDE.local.md")))))
    stop("SELFTEST FAILED: CLAUDE*.md were NOT restored after a build error.")

  message("SELFTEST PASSED: guard catches a leaked page and every index that points at it, ",
          "passes clean sites AND the package's own AI-tool disclosures, and restores ",
          "CLAUDE*.md even when the build errors.")
  invisible(TRUE)
}

## ---- dispatch -------------------------------------------------------------------------------
## The source-only gate lets other scripts source these functions for testing without kicking
## off a real build. Normal CLI use (Rscript --vanilla dev/build_docs.R) leaves it unset.
if (!nzchar(Sys.getenv("BUILD_DOCS_SOURCE_ONLY"))) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--selftest" %in% args) selftest() else build_docs()
}
