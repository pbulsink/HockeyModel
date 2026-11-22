# NEWS.md rules

Applies to step 10 of the standard workflow in [AGENTS.md](../../AGENTS.md). Add one bullet at the top of `NEWS.md`, under the development-version heading.

## Format

- User-facing changes only. One line, ending with `.`
- Present tense, positive framing; function names (backticks + `()`) near the start: `` * `fn()` now accepts ... `` not `* Fixed ...`
- Issue/contributor before the final period: `` * `fn()` now accepts ... (@user, #N). `` where `#N` is the GitHub issue number being implemented (e.g. `#42`).

## Resolving attribution

- Username: `gh api user --jq .login`.
- Issue number: from the user's prompt, the branch name (`git branch --show-current`), or `gh issue list`.
- **Never guess or invent an issue number.** Before writing it, verify that (1) you received it from the user or the branch name, OR (2) you looked it up with `gh`. If you cannot trace the number to a concrete source, use `#noissue`.

## Example

* `inRegularSeason()` now accepts a `league` argument (@pbulsink, #43).
