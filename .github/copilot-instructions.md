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
