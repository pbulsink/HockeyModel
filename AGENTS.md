# AGENTS.md

**HockeyModel** — R package that predicts NHL game winners, season points, and playoff/cup odds; includes PWHL support via the HockeyTech API. https://github.com/pbulsink/HockeyModel

## Conventions

- R console: use `--quiet --vanilla`.
- Always run `air format .` after generating R code.
- Comments explain *why*, not *what*.

## Standard workflow

For any feature, fix, or refactor:

1. **Update packages**: `pak::pak()`
2. **Run tests** — confirm passing before changes: `devtools::test(reporter = "check")`. If any fail, stop and ask.
3. **Plan** — identify affected R files; check if new exports are needed.
4. **Test first** — write failing test, then implement: `devtools::test(filter = "name", reporter = "check")`.
5. **Implement** — minimal code to pass tests.
6. **Refactor** — clean up, keep tests green.
7. **Document** — document any new or changed exports.
8. **Verify**: Run `devtools::test(reporter = "check")`, then `devtools::check(error_on = "warning")`. Resolve warnings, errors, and NOTEs.
9. **Review**: Evaluate the changes you've made as if you're doing a new code review. Action any obvious improvements and highlight the questionable or unclear bits as if from a code review when writing your summary comment.
10. **News** — add a bullet at the top of `NEWS.md` under the dev heading.

## Repository structure

Standard R package layout; see [structure](docs/agent-instructions/structure.md) for the full tree. In `R/`, one file ~= 1 exported function, and `man/` is roxygen-generated.

## Annexes (read before calculation, modelling, or graphics work)

Use these instead of re-exploring the package.

| Topic | Path |
|-------|------|
| NHL model, simulation, playoffs, tuning | [model-nhl](docs/agent-instructions/model-nhl.md) |
| PWHL model and differences from NHL | [model-pwhl](docs/agent-instructions/model-pwhl.md) |
| Graphics, tables, and posting workflow (both leagues) | [graphics-products](docs/agent-instructions/graphics-products.md) |

## Skills

| Triggers | Path |
|----------|------|
| create GitHub issues | @.github/skills/create-issue/SKILL.md |
| document functions | @.github/skills/document/SKILL.md |
| from github | @.github/skills/github/SKILL.md |
| implement issue / work on #NNN | @.github/skills/implement-issue/SKILL.md |
| writing R functions / API design / error handling | @.github/skills/r-code/SKILL.md |
| writing or reviewing tests | @.github/skills/tdd-workflow/SKILL.md |
| search / rewrite code (astgrepr) | @.github/skills/search-code/SKILL.md |
| verify / check package | @.github/skills/cmd-check/SKILL.md |
