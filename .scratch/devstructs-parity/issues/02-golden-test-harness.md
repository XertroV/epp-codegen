# Golden test harness shape

Type: grilling
Status: resolved
Blocked by: 01

## Question

What is the shape of the golden test suite that must hold emit byte-identical to the Editor++ DevStructs?

Sub-questions:

- Where do fixtures live: copied into this repo (`tests/fixtures/*.xtoml` + expected `.as`) vs. referencing the E++ checkout by path? Copies drift; references break off-machine.
- Which seam do goldens cross at first: the process seam (run binary, compare stdout — survives all refactors) vs. an `emit()` library seam (faster, but only exists after Candidate 1 lands)?
- Are the two `/// !` header lines stripped in fixtures, or is the wrapper part of any test?
- Minimum fixture set per AGENT-FIXUP.md: LightMapStructs (Embedded, class comments), CPlugSolid2Model (Struct + NativeClass + string setter), Macroblocks (Inline, Buffer, MwIdValue, enum widths), Viewport (many Struct/Buffer), NSceneAnim_SMgr (offset expressions). More, or is that enough?

## Answer

Resolved by grilling, 2026-08-17:

- **Fixtures live in this repo**: all 27 `codegen/**/*.xtoml` files copied under `tests/fixtures/`, mirroring the source tree, each paired with an expected `.as` body.
- **Expected bodies** = checked-in DevStructs minus the first 3 lines (two `/// !` header lines + one blank line — the Python wrapper's framing, confirmed by ticket 01). Fixtures contain exactly what `epp-codegen` must print to stdout.
- **Process seam, permanently**: integration tests run the built binary (`CARGO_BIN_EXE_epp-codegen`) per fixture and byte-compare stdout. The harness never changes during the module split.
- **Drift is deliberate**: E++ `.xtoml` edits do not auto-propagate into fixtures; refresh policy stays in the map's Not-yet-specified.
- Parity is all-or-nothing: all 27 files, not the brief's five — the one-off behaviors (GameCamera's skipped field, CursorBlock's dropped comments, the empty class) live outside the five.
