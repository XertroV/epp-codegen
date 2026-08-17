# The xtoml format

xtoml is the input language of `epp-codegen`. It describes memory layouts of
Trackmania game objects so the compiler can generate AngelScript `RawBufferElem`
accessor classes (DevStructs) for Openplanet.

Despite the name, **xtoml is not TOML**. It is a line-based format: one
directive per line, no nesting, no escaping, no values with internal structure.
Blank lines are always ignored. One `.xtoml` file compiles to exactly one
AngelScript file on stdout.

A file contains one or more **classes**. Each class starts with a header and is
followed by directives until the next header or end of file.

## Class header

```
[DGameCtnMacroBlockInfo: SZ_CTNMACROBLOCK]
[DPlugBitmap: 448]
[DSnapCam : 0x120]
```

`[Name: SIZE]` — spaces around the colon are allowed. `SIZE` is copied
**verbatim** into the generated size checks and `super(...)` calls. It may be:

- a hex literal (`0x2E8`, `0x120`),
- a decimal literal (`448`, `29248`),
- or a token defined elsewhere in AngelScript (`SZ_CTNMACROBLOCK`,
  `O_SOLID2MODEL_PRELIGHT_GEN`).

The generated class always gets two constructors:

```angelscript
class DPlugBitmap : RawBufferElem {
	DPlugBitmap(RawBufferElem@ el) {
		if (el.ElSize != 448) throw("invalid size for DPlugBitmap");
		super(el.Ptr, el.ElSize);
	}
	DPlugBitmap(uint64 ptr) {
		super(ptr, 448);
	}
```

## Directives

### Fields

```
name = TYPE, OFFSET, ACCESS[, RWTYPE]
```

- `OFFSET` is verbatim text: `0xA8`, `0`, `0x40+0x20`,
  `O_ITEMCURSOR_Helper - 0x8`, `(0x38 + 0x24)` all pass through untouched.
- `ACCESS` is exactly `G` (getter), `S` (setter), or `GS` (both). A line whose
  third column is anything else is **skipped** (with a warning on stderr) —
  see [Leniency](#leniency).
- `RWTYPE` (optional 4th column) overrides the `Get*`/`Set*` vocabulary. The
  only use in practice is `MwIdValue` on a `string` field, which reads/writes
  the string as a MwId. The property's AngelScript type stays `string`.

What a field emits depends on how `TYPE` is classified (see
[Type classification](#type-classification)):

```angelscript
// score = uint, 0xA8, GS
	uint get_score() { return (this.GetUint32(0xA8)); }
	void set_score(uint value) { this.SetUint32(0xA8, value); }

// Path = string, 0x20, GS
	string get_Path() { return (this.GetString(0x20)); }
	void set_Path(const string &in value) { this.SetString(0x20, value); }

// name = string, 0, GS, MwIdValue
	string get_name() { return (this.GetMwIdValue(0)); }
	void set_name(const string &in value) { this.SetMwIdValue(0, value); }

// Material = CPlugMaterial, 0x120, GS        (a nod)
	CPlugMaterial@ get_Material() { return cast<CPlugMaterial>(this.GetNod(0x120)); }
	void set_Material(CPlugMaterial@ value) { this.SetNod(0x120, value); }

// dir = CGameCtnBlock::ECardinalDirections(4), 0x58, GS   (an enum)
	CGameCtnBlock::ECardinalDirections get_dir() { return CGameCtnBlock::ECardinalDirections(this.GetUint32(0x58)); }
	void set_dir(CGameCtnBlock::ECardinalDirections value) { this.SetUint32(0x58, value); }
```

Note the details — they are contractual:

- Value-type getters wrap the read in extra parens: `return (this.GetUint32(...));`
- Nod and enum getters do **not** have the extra parens; they cast instead.
- `string` setters take `const string &in value` (including `MwIdValue`
  fields); nod setters take `Type@ value`.
- Enum setters store the raw value with no cast.

### NativeClass

```
NativeClass = CPlugBitmap
```

Adds a constructor from a live game nod, plus a `get_Nod()` accessor:

```angelscript
	DPlugBitmap(CPlugBitmap@ nod) {
		if (nod is null) throw("not a CPlugBitmap");
		super(Dev_GetPointerForNod(nod), 448);
	}
	CPlugBitmap@ get_Nod() {
		return cast<CPlugBitmap>(Dev_GetNodFromPointer(ptr));
	}
```

### Struct: — pointer to another generated struct

```
Struct: RenderInfo = DRenderInfo, 0xA8, G
```

Emits a **null-safe** getter reading a pointer at the offset:

```angelscript
	DRenderInfo@ get_RenderInfo() { auto _ptr = this.GetUint64(0xA8); if (_ptr == 0) return null; return DRenderInfo(_ptr); }
```

The offset is verbatim (`0x40+0x20` is fine). Setter access (`S`) is
unimplemented: it produces a warning, never a setter.

### Embedded: — in-place struct

```
Embedded: LightMapParams = DHmsLightMapParam, 0x100, G
```

Like `Struct:`, but the struct lives *inside* the parent at
`this.Ptr + offset`, so there is no pointer read and no null check:

```angelscript
	DHmsLightMapParam@ get_LightMapParams() { return DHmsLightMapParam(this.Ptr + 0x100); }
```

Setter access warns, as with `Struct:`.

### Buffer: — buffer field + wrapper class

```
Buffer: Blocks = DGameCtnMacroBlockInfo_Blocks, O_MACROBLOCK_BLOCKSBUF, SZ_MACROBLOCK_BLOCKSBUFEL, true
```

Columns: name, wrapper class, offset, element size, `behindPtr` flag
(optional; defaults to `false`). Two things are emitted.

The getter — **deferred**: all buffer getters emit at the *end* of the class
body, after every field and `Inline:` line, regardless of where the `Buffer:`
directive appears:

```angelscript
	DGameCtnMacroBlockInfo_Blocks@ get_Blocks() { return DGameCtnMacroBlockInfo_Blocks(this.GetBuffer(O_MACROBLOCK_BLOCKSBUF, SZ_MACROBLOCK_BLOCKSBUFEL, true)); }
```

And a wrapper class, emitted immediately after the containing class:

```angelscript
class DGameCtnMacroBlockInfo_Blocks : RawBuffer {
	DGameCtnMacroBlockInfo_Blocks(RawBuffer@ buf) {
		super(buf.Ptr, buf.ElSize, buf.StructBehindPtr);
	}
	DGameCtnMacroBlockInfo_Block@ GetBlock(uint i) {
		return DGameCtnMacroBlockInfo_Block(this[i]);
	}
}
```

The wrapper naming rule is **load-bearing**: the wrapper name must end in `s`.
The element type is the wrapper name minus the trailing `s`; the accessor is
`Get` + the element type's last `_` segment (or the whole name if there is no
`_`): `DGameCtnMacroBlockInfo_Blocks` → element `DGameCtnMacroBlockInfo_Block`
→ `GetBlock`; `DSystemFidFiles` → `GetDSystemFidFile`; `DGameCtnGhost_CPs` →
`GetCP`. A wrapper name not ending in `s` is an **error** (exit code 1).

### Inline: — raw AngelScript

```
Inline: vec3 get_pyr() { return vec3(ypr.y, ypr.x, ypr.z); }
```

The rest of the line is emitted verbatim at one tab, in field-stream order.
Used for bitfield accessors, aliases, constants — anything the field syntax
can't express. Comments above an `Inline:` line are consumed but **not**
emitted (historical behavior).

### Comments

```
# CacheSize + 0x80
```

A `#` line becomes a `//` comment in the output (a bare `#` becomes `// ` with
a trailing space). Attachment rules:

- `#` lines immediately above a `[Class: SIZE]` header attach to the **class**
  and are emitted at column 0 before `class Name`. Blank lines between the
  comment and the header do not break attachment.
- `#` lines above any other directive attach to that directive and are emitted
  at one tab above the generated accessor.
- Comments with **no following directive** (end of class, end of file) are
  silently dropped.

### Blank lines

Always ignored. They never produce blank lines in the output.

## Type classification

The field's `TYPE` is classified by two rules:

| `TYPE` shape | Kind | Accessor vocabulary |
|---|---|---|
| Capitalized, contains `:` (e.g. `CGameCtnBlock::ECardinalDirections`) | enum | width from `(N)` suffix: `(1)` → `Uint8`, `(2)` → `Uint16`, `(4)` or absent → `Uint32` |
| Capitalized, no `:` (e.g. `CGameCtnBlockInfo`, `ISceneVis`) | nod | `GetNod`/`SetNod`, `@` handle, `cast<T>` |
| Anything else | scalar | table below |

Scalar accessor mapping (the declared token is echoed as the AngelScript type;
the `Get*`/`Set*` suffix derives from it):

| xtoml type | Suffix | | xtoml type | Suffix |
|---|---|---|---|---|
| `uint`, `uint32` | `Uint32` | | `vec2` / `vec3` / `vec4` | `Vec2` / `Vec3` / `Vec4` |
| `int`, `int32` | `Int32` | | `iso4` | `Iso4` |
| `uint8` / `uint16` / `uint64` | `Uint8` / `Uint16` / `Uint64` | | `mat3` | `Mat3` |
| `int8` / `int16` / `int64` | `Int8` / `Int16` / `Int64` | | `nat2` / `nat3` | `Nat2` / `Nat3` |
| `bool` | `Bool` | | `quat` | `Quat` |
| `float` | `Float` | | `string` | `String` |
| any other lowercase word | first letter uppercased | | | |

## Leniency

Two historical behaviors are deliberate and relied upon by the existing corpus:

- A field line whose third column is not exactly `G`, `S`, or `GS` (e.g.
  `UnkCameraThing = uint, 0x1AC, 0x15`) is **skipped**: it produces no output,
  any pending comment is dropped with it, and a warning is printed to stderr.
  The exit code stays 0.
- Comments with no following directive vanish silently.

## Output layout contract

- Tabs, never spaces. Fields indent one tab; bodies two.
- Class body: constructors, one blank line, then members. An empty class gets
  one blank line between the last constructor and `}`.
- Members emit in source order, except buffer getters (always last in the body)
  and buffer wrapper classes (immediately after the containing class).
- Blank lines between top-level blocks: class → class = 2; class → its first
  wrapper = 1; wrapper → wrapper = 2; last wrapper → next class = 1.
- A file ending in a plain class ends `}` + two blank lines; a file ending in a
  buffer wrapper ends `}` + one blank line.
- The two-line `/// ! This file is generated from ...` header seen in checked-in
  DevStructs is **not** part of this tool's output — the Editor++ wrapper
  (`run_codegen.py`) prepends it.

## A complete example

Input (`LightMapStructs.xtoml`):

```
# from _SPimp, at 0x100 there is an embedded CHmsLightMapParam node
# SZ_NHmsLightMap_SPImp = 0x4E0;
[D_NHmsLightMap_SPImp: 0x4E0]
# CacheSize + 0x80
Embedded: LightMapParams = DHmsLightMapParam, 0x100, G
```

Output:

```angelscript
// from _SPimp, at 0x100 there is an embedded CHmsLightMapParam node
// SZ_NHmsLightMap_SPImp = 0x4E0;
class D_NHmsLightMap_SPImp : RawBufferElem {
	D_NHmsLightMap_SPImp(RawBufferElem@ el) {
		if (el.ElSize != 0x4E0) throw("invalid size for D_NHmsLightMap_SPImp");
		super(el.Ptr, el.ElSize);
	}
	D_NHmsLightMap_SPImp(uint64 ptr) {
		super(ptr, 0x4E0);
	}

	// CacheSize + 0x80
	DHmsLightMapParam@ get_LightMapParams() { return DHmsLightMapParam(this.Ptr + 0x100); }
}
```

## Diagnostics and exit codes

| Condition | Severity | Exit code |
|---|---|---|
| Buffer wrapper name not ending in `s` | Error | 1 |
| Malformed directive columns | Error | 1 |
| `S` access on `Struct:` / `Embedded:` | Warning | 0 |
| Field line with invalid access column (skipped) | Warning | 0 |
| Dangling comments | silent | 0 |
| Unreadable input file | — | 1 |
| Invalid command line | — | 2 |

Warnings and errors print to stderr; stdout always carries the best-effort
generated file and nothing else.
