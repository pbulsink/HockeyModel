GitHub Copilot Chat Assistant

---
name: cmd-check
trigger: run R CMD check / check package / validate package
description: Enforce and explain R CMD check for R packages. Use when verifying code quality before merging or completing a task.
---

# R CMD check

Run `R CMD check` to ensure the package structure, documentation, and code meet CRAN-like quality standards. Per the standard workflow in `AGENTS.md`, this should be run after tests pass.

## Running checks

Use `devtools::check()` for the standard development loop. Use `rcmdcheck` for more detailed programmatic control or CI-like local runs.

```r
# Standard check (as defined in AGENTS.md)
devtools::check(error_on = "warning")

# Comprehensive CRAN-style check
rcmdcheck::rcmdcheck(args = "--as-cran")
```

## Interpreting results

| Level | Action | Description |
|-------|--------|-------------|
| **ERROR** | **Blocker** | Must be fixed immediately. Includes failing tests, broken `NAMESPACE`, or invalid syntax. |
| **WARNING** | **High Priority** | Should be fixed before merging. Includes missing imports, documentation mismatches, or invalid cross-references. |
| **NOTE** | **Review** | Non-blocking but should be minimized. Includes large file sizes, missing `Title`/`Description` fields, or global variable issues. |

## Common fixes

### Missing dependencies
Add missing packages to the `Imports` (runtime) or `Suggests` (test/doc only) section of `DESCRIPTION`. Ensure all external functions are called using `package::function()`.

### Documentation out of sync
If `Rd` files or `NAMESPACE` are flagged, run:
```r
devtools::document()
```
Then re-run the check.

### Global variable NOTES
If `check` warns about "no visible binding for global variable", add the variables to `R/HockeyModel-package.R` or use `utils::globalVariables()` to quiet the note, especially when using `dplyr` or `data.table`.

### Examples failing
If examples take too long or require internet access, wrap them in `\dontrun{}` or `\donttest{}` in the roxygen comments. Use `@examplesIf interactive()` for network-dependent functions.

## Best practices

- **Check locally before PR**: Always run `devtools::check()` before pushing to ensure CI passes.
- **Clean environment**: Run checks in a fresh R session to avoid contamination from the global environment.
- **Namespace hygiene**: Ensure `NAMESPACE` only exports intended functions. Use `@keywords internal` to hide helpers from the documentation index while keeping them exported if necessary.
- **News update**: If fixing a check issue that affects users, add a note to `NEWS.md`.

## References

- rcmdcheck documentation
- CRAN Repository Policy
- r-lib GitHub Actions