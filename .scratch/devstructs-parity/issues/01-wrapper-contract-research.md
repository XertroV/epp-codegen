# Wrapper contract research

Type: research
Status: resolved
Blocked by:

## Question

What exactly does the Editor++ side require of the `epp-codegen` process? Gather the facts every other ticket depends on:

- `run_codegen.py`: exact argv/cwd when invoking `-e epp-codegen`; stdout/stderr handling; the two-line header's `up_dirs` path math; `.xtoml` → `.as` path mapping; behavior on non-zero exit.
- `build.sh`: the `epp-codegen --version` gate.
- Corpus survey: which directives each of the 27 `codegen/**/*.xtoml` files actually uses (Buffer / Struct / Embedded / Inline / NativeClass / enum widths / MwIdValue overrides / offset expressions / comments above class headers).
- Golden whitespace spot-checks (Struct null-guard, Embedded getter, Buffer wrapper pair).

## Answer

Full report: [../assets/01-wrapper-contract-report.md](../assets/01-wrapper-contract-report.md). Headlines:

- **Invocation confirmed**: `epp-codegen <abs path to one .xtoml>`, stdout captured verbatim as the file body after a 2-line header + 1 blank line; stderr passes through uninspected; `check=True` means any non-zero exit kills the run before the file is written.
- **`build.sh` gate**: `--version` needs any non-empty stdout; only exit codes matter per file.
- **New byte-parity details beyond AGENT-FIXUP.md**: body ends `}\n\n\n`; blank-line separators between top-level blocks follow a class→class=2 / class→first-wrapper=1 / wrapper→wrapper=2 rule; **buffer getters are deferred to the end of the class body** (fields + Inline stay in source order); **invalid field lines are silently skipped** (3rd column not G/GS → no output, no error, comment dropped too — must reproduce); enum getters cast without parens; nod fields with GS get `SetNod` setters; empty classes get two blank lines after ctors.
- **Wrapper deps**: system Python is PEP-668 managed; created venv `~/.venvs/epp-codegen` with `termcolor` + `click`. Run the wrapper as `~/.venvs/epp-codegen/bin/python ./run_codegen.py ...`.
- Corpus survey table (directive usage per file, token SIZEs, offset expressions, class-header comment sites) is in the asset report.
