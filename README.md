# epp-codegen

Generates AngelScript **DevStructs** — `RawBufferElem`/`RawBuffer` accessor
classes for poking at Trackmania game memory from Openplanet — from compact
`.xtoml` layout specs. Built for
[Editor++](https://github.com/XertroV/tm-editor-plus-plus); its golden standard
is byte-identical regeneration of the DevStructs checked into that repo.

```
[DPlugBitmap: 448]                                    class DPlugBitmap : RawBufferElem {
NativeClass = CPlugBitmap                                 DPlugBitmap(RawBufferElem@ el) { ... }
Struct: RenderInfo = DRenderInfo, 0xA8, G           →       ...
                                                          DRenderInfo@ get_RenderInfo() { auto _ptr = this.GetUint64(0xA8); if (_ptr == 0) return null; return DRenderInfo(_ptr); }
                                                      }
```

## Install

```bash
just install          # or: cargo install --path .
```

## Usage

```bash
epp-codegen path/to/spec.xtoml > out.as
epp-codegen --version
```

One positional argument (the `.xtoml` file); the generated AngelScript goes to
stdout, diagnostics to stderr.

| Situation | Exit code |
|---|---|
| Success (warnings possible) | 0 |
| Error diagnostics, or unreadable input file | 1 |
| Bad command line (clap) | 2 |

Stdout is always exactly the best-effort generated file — nothing else is ever
printed there, so it is safe to pipe.

## Editor++ integration

Editor++ drives the tool through its own wrapper:

```bash
# in the tm-editor-plus-plus repo
python3 ./run_codegen.py ./codegen ./src/DevStructs/ -e epp-codegen
git diff -- src/DevStructs     # empty = byte-parity
```

`run_codegen.py` walks `codegen/`, invokes `epp-codegen` once per `.xtoml`, and
prepends the two-line `/// ! This file is generated ...` header itself — the
header is **not** part of this tool's output. `build.sh` probes
`epp-codegen --version` to decide whether to run codegen at all, so the binary
must be on `PATH` (`just install` puts it in `~/.cargo/bin`).

## The xtoml format

See **[docs/xtoml.md](docs/xtoml.md)** for the full language reference:
class headers, field syntax, `NativeClass` / `Struct:` / `Embedded:` /
`Buffer:` / `Inline:` directives, comment attachment, type classification, and
the exact emission contract (tabs, null-guards, deferred buffer getters,
wrapper naming rules).

Despite the extension, xtoml is **not TOML** — it is a line-based format.

## Development

```bash
just test             # golden suite: 27 corpus fixtures, byte-compared via the binary
just build / release
just lint / fmt
just gen spec.xtoml   # run the compiler on a spec
```

The golden tests (`tests/golden.rs`) run the built binary against every fixture
in `tests/fixtures/` and byte-compare stdout with the paired `.expected.as`.
Fixtures are deliberate snapshots of the Editor++ corpus (checked-in DevStructs
with the wrapper's 3-line header stripped); refreshing them is a conscious act,
not an automatic one.

Architecture (see `CONTEXT.md` for the domain vocabulary):

```
main.rs    CLI adapter only — clap, --version, exit codes, stderr rendering
parse.rs   xtoml text -> (FileAst, Vec<Diagnostic>)   line state machine
ast.rs     the types + type classification (Ty, accessor_suffix — one quirk home)
emit.rs    FileAst -> (String, Vec<Diagnostic>)       all formatting lives here
error.rs   Diagnostic { severity, line, message }
```

Design history and the decision record live in `.scratch/devstructs-parity/`
(a completed wayfinder map).
