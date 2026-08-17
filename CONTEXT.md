# epp-codegen domain model

Domain vocabulary for this repo. Use these terms exactly in conversation, tickets, and code comments.

- **xtoml** — the line-based input format parsed by this tool. Not real TOML. Directives: `[Class: SIZE]`, `NativeClass`, field lines, `Buffer:`, `Struct:`, `Embedded:`, `Inline:`, `#` comments. Reference: `AGENT-FIXUP.md` and `.scratch/devstructs-parity/assets/01-wrapper-contract-report.md`.
- **DevStructs** — the generated AngelScript output files, checked in at the Editor++ repo under `src/DevStructs/`. They are the *newer* compiler's output and the spec this tool must byte-reproduce; never edit them to match the tool.
- **Golden corpus** — the 27 `.xtoml` files under Editor++ `codegen/` plus their checked-in DevStructs; the all-or-nothing parity target.
- **Golden fixtures** — the in-repo copies of the corpus under `tests/fixtures/` (xtoml + expected `.as` body). Refreshed deliberately, never automatically.
- **Wrapper** — Editor++ `run_codegen.py`; invokes `epp-codegen` per file and prepends the two `/// !` header lines plus one blank line. The header is the wrapper's job, not the tool's.
- **Process seam** — the CLI contract (one positional path, AS on stdout, errors on stderr, exit code). The golden tests cross this seam and nothing else.
- **Parity** — byte-identical regeneration: wrapper run + `git diff -- src/DevStructs` is empty.

Architecture terms (module, interface, depth, seam, adapter, leverage, locality) come from the `/codebase-design` skill glossary and are not redefined here.

## Current state

- **Byte-parity achieved** (2026-08-17): all 27 golden fixtures green; a real wrapper run leaves `git diff -- src/DevStructs` empty.
- CLI contract implemented: clap, `--version` with git hash, exit codes per the error model.
- Wayfinder map: `.scratch/devstructs-parity/map.md` — **all tickets resolved**; only fog (drift policy, CI, distribution) remains unticketed.
- Architecture direction (decided, in implementation): module split into main(CLI adapter)/parse/ast/emit/error per the ticket-04 hybrid AST design; goldens stay at the process seam.
