# Bloom Native

*Formerly Grove Native.*

Bloom's Elisp implementation for Jetpacs. Emacs Org owns files, state, parsing,
search, capture and mutations; Jetpacs provides native presentation and durable
transport. The Android reference implementation is
[Bloom](https://github.com/rrajath/bloom) (formerly Grove), checked out
locally in [`../grove/`](../grove/).

The checkout is `bloom-native`; Elisp features are `bloom-*`, and the
registered app and packaged feature are `bloom`. Installing or enabling the
applet is still an explicit user choice.

## Rename from Grove

Upstream renamed the Android app from Grove to Bloom on 2026-10, and this
applet followed in full: checkout, Elisp features and symbols, wire actions
(`bloom.*`), surfaces (`app:bloom`, `widget:bloom.*`), the Customize group,
design presets (`bloom.default`, `bloom.light`, `bloom.dark`), and Jetpacs'
packaging. Former `grove-*` Customize options remain obsolete aliases, and
stored `grove-` ids are rewritten on the next library refresh. Home-screen
widgets pinned under the old surface ids must be re-added.

## Layout and dependencies

| Path | Owns |
|---|---|
| `elisp/` | Bloom applet implementation and design presets |
| `test-elisp/` | Hermetic ERT, Checkdoc and temporary compilation runner |
| [ELISP.md](ELISP.md) | Implemented workflows, configuration and state boundaries |
| [LICENSE](LICENSE) | Apache License 2.0 inherited with the source |

The normal layout uses sibling `jetpacs-poc`, `ebp.el`, `ebp-org` and
the `jetpacs-poc/jetpacs-components` module under `~/workspace/`. Run:

```sh
./test-elisp/run-tests.sh
```

`JETPACS_REPOSITORIES_ROOT` selects another collection directory. The existing
`JETPACS_ROOT`, `EBP_EL_ROOT`, `EBP_ORG_ROOT` and `JETPACS_COMPONENTS_ROOT`
overrides select individual dependencies.

Jetpacs packages `bloom-native/elisp` into the device `apps/bloom`
directory. This repository has no Android build or backend implementation.

## Extraction history

Created locally on 2026-09-05 from Grove's `main` branch. The history for
`elisp/`, `test-elisp/`, `ELISP.md` and the license was imported into this
independent Git repository. Commit IDs changed because the Android tree was
excluded; authors, timestamps and commit messages were retained. Grove's original history
and remote remain intact. Existing navigation and test edits were carried over
as uncommitted changes. The README's Jetpacs outline section moved to `ELISP.md`.

No remote is configured for this checkout. The original/extracted object map
and a pre-extraction backup are recorded in the local migration backup.

## Local verification and recovery

The extracted checkout passed all 41 ERT tests, Checkdoc and warning-as-error
compilation into a temporary directory. Jetpacs' onboarding asset task, focused
onboarding unit tests and shell layout fixtures passed; the packaged payload
matches all 12 Elisp sources byte-for-byte. No device deployment was performed.

The preservation audit found no missing files and confirmed the original Grove
HEAD and Android files were unchanged. All 16 extracted commits retain their
authors, timestamps and messages. Original signatures remain in the original
history because extracted commits have different trees and IDs.

Local backup:

```text
/home/calebc42/workspace/.grove-native-extraction-backup-20260905-215335/
  original-git/          original Git database
  original-README.md     original dirty README
  removed/              complete original Elisp/test/doc trees
  baseline.json         original file hashes and Git status
  history-map.json      original-to-extracted history mapping
  verification/         source audit and test logs
```

The extraction edits are left uncommitted in Grove, Grove Native and Jetpacs.
No remote was changed and no repository was pushed.
