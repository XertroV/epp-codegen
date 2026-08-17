# CLI contract

Type: grilling
Status: resolved
Blocked by: 03

## Question

What is the exact process-edge contract of `epp-codegen`?

**Accepted (2026-08-17):** CLI becomes a thin adapter (architecture review candidate 4). Sub-questions below settle at implementation time, post-parity.

Constraints already set by AGENT-FIXUP.md + E++ `build.sh`:

- `--version` must print something non-empty (`epp-codegen 0.1.0` is fine) or build.sh treats the tool as missing.
- Exactly one positional `.xtoml` path; generated AS on stdout; errors on stderr; exit non-zero if any errors.

Sub-questions:

- clap vs hand-rolled argv handling (one positional + one flag is near the clap threshold; clap gets `--version`/`--help` for free but adds a dep — brief says don't churn deps mid-fix, so this lands post-parity).
- Exact `--version` string and exit behavior for unknown args / missing file.
- Stdout purity: nothing but the generated file ever goes to stdout (warnings included).

## Answer

Resolved by grilling, 2026-08-17:

- **clap** for argv parsing (lands post-parity).
- **`--version`** prints `epp-codegen <version> (<short-hash>)` — version from `CARGO_PKG_VERSION`, hash from a `build.rs` running `git rev-parse --short HEAD`, with graceful fallback when git/repo is unavailable.
- **Argv failures** (unknown flags, extra positionals): clap standard — usage to stderr, exit 2. **File failures**: `error: cannot read <path>: <reason>` on stderr, exit 1, file named explicitly (matters in 27-file wrapper runs). Nothing on stdout in either case.
- **Stdout purity**: nothing but the generated file on stdout; warnings and errors ride stderr (per ticket 03).
