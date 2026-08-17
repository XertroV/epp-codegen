# Wayfinder map: DevStructs byte-parity, then architecture split

Labels: wayfinder:map
Tracker: local markdown (`.scratch/`)

## Destination

`epp-codegen` regenerates the checked-in Editor++ DevStructs byte-identically
(`cd ~/src/openplanet/my-plugins/tm-editor-plus-plus && python3 ./run_codegen.py ./codegen ./src/DevStructs/ -e epp-codegen && git diff -- src/DevStructs` is empty),
exits non-zero on errors, answers `--version` — and only then is split into
parse / AST / emit / diagnostics modules with the goldens green throughout.

## Notes

- The full brief is `AGENT-FIXUP.md` (repo root) — read it first every session. It contains the xtoml language reference, the emit contract, and the prescribed work order.
- Sequencing constraint: **goldens before refactor.** No module split until emit byte-matches; do not drift emit while restructuring.
- DevStructs are the *newer* compiler's output; this repo is the older source. Change the compiler to match DevStructs, never the reverse.
- Architecture vocabulary for the refactor tickets: `/codebase-design` glossary (module, interface, depth, seam, adapter, leverage, locality). Candidate deepenings: architecture review HTML of 2026-08-17 (`/tmp/architecture-review-*.html`; regenerate if gone).
- `main01.rs` is a fossil first draft — delete it during the module split; do not extend it.
- Test loop per the brief: generate to a tmpdir and `diff -u` against `src/DevStructs/`, or run the real wrapper (via venv: `~/.venvs/epp-codegen/bin/python ./run_codegen.py ./codegen ./src/DevStructs/ -e epp-codegen`) and check `git diff`.

## Decisions so far

<!-- one line per closed ticket: gist + link -->

- [Wrapper contract research](issues/01-wrapper-contract-research.md) — invocation/header/exit-code contract confirmed; new parity rules found (deferred buffer getters, block blank-line separators, silent-skip of invalid fields, `}\n\n\n` trailer); wrapper runs via venv `~/.venvs/epp-codegen`.
- [Golden test harness shape](issues/02-golden-test-harness.md) — all 27 files copied to `tests/fixtures/` with expected bodies (DevStructs minus the 3 wrapper framing lines); goldens cross the process seam permanently; fixture drift is deliberate.
- [Error model and diagnostics](issues/03-error-model.md) — three tiers (error → exit 1; warn → stderr only, incl. skipped invalid fields; silent only for orphan comments); `Diagnostic { severity, line, message }` returned as `Vec` from parse/emit, no global; stdout always prints; colorful confined to the CLI adapter.
- [CLI contract](issues/06-cli-contract.md) — clap; `--version` = `epp-codegen <CARGO_PKG_VERSION> (<git short-hash>)` via build.rs; argv errors → clap usage + exit 2; unreadable file → named error + exit 1; stdout carries only the generated file.
- (pre-chart, from AGENT-FIXUP.md) Stay a Rust CLI; stdout = one generated file; no TOML crate; no plugin system.
- (pre-chart, from AGENT-FIXUP.md) Buffer wrapper class names must end in `s`; inner type is name minus `s`. Load-bearing.
- (pre-chart, from AGENT-FIXUP.md) The two-line `/// !` header is the Python wrapper's job, not epp-codegen's.
- (2026-08-17, user) All four architecture-review candidates accepted for implementation, post-parity, in ranked order: 1. emit seam (`emit(&FileAst) -> String`), 2. parse module, 3. diagnostics replacing `static mut ERRORS`, 4. CLI thin adapter (`--version`, exit codes). Remaining shape questions live in tickets 03–06 and settle at implementation time.

## Not yet specified

- Drift policy: how future E++ `.xtoml` edits flow back into this repo's golden fixtures (manual copy? CI check against the E++ checkout?).
- CI: no CI exists in this repo; whether `cargo test` goldens get wired anywhere.
- Whether new xtoml features beyond current E++ usage ever land (e.g. Embedded setters — currently warn-only like Struct).
- Versioning/distribution of `epp-codegen` beyond `cargo install --path .` for E++ `build.sh` consumption.

## Out of scope

- Changing E++ `.xtoml` sources to paper over the tool (from AGENT-FIXUP.md).
- Regenerating and committing "close enough" DevStructs (from AGENT-FIXUP.md).
- X/Z map-cache work, Openplanet plugin code, MemPatcher (from AGENT-FIXUP.md).
- Implementing unused Struct setters — warn only, as today (from AGENT-FIXUP.md).
