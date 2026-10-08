# Working in this repository

## Commits, branches and pull requests

- **Commit as you go.** Make a commit for each logical step that builds and passes its tests. Don't leave a large set of changes uncommitted.
- **Use a feature branch for each pull request.** Don't commit to `master`. Name branches for what they change, in kebab-case (for example `paragraph-table-rows`).
- **Stack dependent pull requests with `gh stack`** (the `github/gh-stack` extension). When a change builds on another that isn't merged yet, it goes on the same stack, not in one large PR.
  - `gh stack init <branch>` starts a stack on `master`. `gh stack add <branch>` adds a layer on top of the current branch.
  - Commit each layer's changes on its own branch.
  - `gh stack submit` pushes every branch, sets each PR's base to the branch below it, and links the PRs as a stack on GitHub. Use `--auto` when no one is at the terminal.
  - If PRs are opened first with `gh pr create --base <branch below>`, `gh stack submit` links them instead of creating new ones.
  - `gh stack view` shows the stack. `gh stack sync` brings it up to date with the remote.
- Independent changes go in their own PR, based on `master`, outside the stack.

## Commit messages and PR descriptions

- Commit titles start with the area changed, then say what changed in plain words (for example `Table grid: one row a paragraph, where the table-structure model missed rows`).
- PR descriptions say what was wrong, what changed, and how it was tested. Name which tests fail without the fix. Note any tests that fail on `master` too.

## Pull requests

- **Open them ready for review, not as drafts:** Copilot code review skips drafts. Re-read the issue (and its comments) right before starting it; it may have changed.
- **Check the stack first.** Before adding to a stack, see whether the PRs below have merged (`gh pr view <n> --json state`). If they have, start from `master` instead. Other sessions may stack PRs on yours too.
- **Keep a stack on `master`:** when PRs below merge, or `master` moves, rebase the rest with `git rebase --update-refs master` (from the top branch) and push each branch with `--force-with-lease`.
- **Before merging,** answer every review thread: fix it on the same PR, or reply saying why not, then resolve it. Merge a stack with `gh stack merge <top PR> --yes --rebase`, and a single PR with `gh pr merge <n> --rebase`.

## Code and tests

- Everything is F#, in modules; tests are xUnit v3 with FsUnit.
- **Name tags, content operators and destination kinds by their constants** (`Tags.Figure`, `ContentOperators.BDC`, `Destinations.FitR`), not string literals, so a misspelt one doesn't compile. Tests can write them as literals: they check what's written.
- **Tests build their PDFs themselves** (iText). Never commit real documents or screenshots of them: the repository will be public.
- The test projects can't see internal modules. Test through the public API: `VeraPdfValidator().Validate`, `.Fix`, `.ReadStructureTree`.
- **A test for a guard should fail without it.** Take the guard out once and see the test fail.
- Run all three test projects before pushing: `tests/Legibyl.Validation.Tests`, `tests/Legibyl.Cli.Tests` and `tests/Legibyl.Gui.Tests`.
- Adding or changing a Legibyl warning (`Structure-N`) or an auto fix: follow `.claude/skills/add-check/SKILL.md`, which lists every place each is wired in. A warning's number is never reused.

## Wording and scope

- What people read is plain language about what it means for someone using a screen reader, not PDF jargon ("MCID", "OBJR", "StructTreeRoot"). It says "fix", never "repair" (`FixNamesTests` checks this).
- Keep the README's tables (checks, fixes) in step with the code; `FixNamesTests` checks the fix table.
- Build new structural auto fixes (retagging, role mapping, marking artifacts) only when asked for.

## What reviews should look for

- **A fix must never change what a page draws,** nor lose or change text a screen reader reads (`/Alt`, `/ActualText`, `/Lang`, marked content's text).
- **The tags stay consistent:** each tag's `/P` and its place in its parent's `/K`, `/Pg`, MCIDs (unique on a page), and the parent tree. Content taken out of the tags is marked as an artifact and untied from the parent tree, not left tied to a tag that's gone.
- **When unsure, refuse and say why** (in plain words), rather than guess at reading order or meaning. A warning offers a fix only where the fix would act.
