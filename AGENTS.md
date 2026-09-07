# Grove Native implementation guide

This repository owns Grove's Jetpacs Elisp applet and tests. The sibling
`../grove/` repository owns the independent Android application, which remains
a visual and workflow reference. Read `ELISP.md` before changing behavior.

- Preserve unrelated working changes. Inspect this repository's own Git status.
- EBP protocol authority is `../ebp/SPEC.md`; contract JSON and generated code
  are projections. Jetpacs foundation belongs to `../jetpacs-poc/`.
- Emacs Org owns application state and mutations. Builders return the actual
  deterministic screen plist/vector; effects belong in actions or producers.
- Keep `grove-*` features and the `grove` app identity stable unless explicitly
  requested. A checkout rename does not rename runtime identifiers.
- For applet behavior or trust changes, read the owning Jetpacs guides in
  `../jetpacs-poc/jetpacs-applet-mcp/APPLET-GUIDE.md` and `DETERMINISM.md`.
- Run `./test-elisp/run-tests.sh` for ERT, Checkdoc and warning-as-error byte
  compilation. Compile only into temporary directories; never overwrite source
  bytecode. Device verification is required for claims about device behavior.
- Do not execute untrusted Elisp or add inspection-only Org source to runtime
  load paths. Local fixture tests use the runtime-compatible installed Org.
