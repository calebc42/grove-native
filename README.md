# Bloom Native

Bloom Native rebuilds the [Bloom](https://github.com/rrajath/bloom) Android
app (formerly Grove) screen for screen, with Emacs doing the work. The app's
screens are written in Emacs Lisp. Emacs Org owns the files, parsing, search,
capture and every edit, so your notes stay plain `.org` files that Emacs
reads and writes directly.

On the phone, a native Android app called the **Jetpacs Companion** draws
those screens with Jetpack Compose. Emacs sends it each screen as a small
JSON tree, and it sends taps and edits back. The two talk over the **Emacs
Bridge Protocol (EBP)**. Emacs can run on the phone itself, which is the
setup this README walks through, or on a desktop.

```text
 Android phone
 ┌──────────────────────────────┐      ┌──────────────────────────────┐
 │ Jetpacs Companion (Compose)  │ EBP  │ Android Emacs                │
 │ draws screens, sends taps    │◀────▶│ Bloom Native (this repo)     │
 │ and edits                    │      │ + Org mode, owns your files  │
 └──────────────────────────────┘      └──────────────────────────────┘
```

The goal is pixel parity with Bloom 1.11. [PARITY.md](PARITY.md) lists what
already matches on a device and what remains.

## What you need

- **An Android phone** on Android 14 or later, with USB or wireless debugging
  turned on.
- **Emacs for Android**, version 30 or later. It's published by the GNU
  project and its package name is `org.gnu.emacs`. Get it from F-Droid or the
  GNU Emacs Android builds on SourceForge.
- **A computer** to build the Companion, with:
  - Git;
  - JDK 21;
  - the Android SDK, through Android Studio or the command-line tools;
  - `adb`.
- **Emacs 30 or later on the computer**, only if you want to run the tests.

## 1. Get the code

Jetpacs builds from several sibling repositories that sit in one directory.
Two of them must be cloned under a different directory name than their
GitHub repository: `ebp-poc` goes in `ebp`, and `grove-native` goes in
`bloom-native`. The build finds the directory by looking for `ebp/SPEC.md`
next to `jetpacs-poc`.

```sh
mkdir bloom-workspace && cd bloom-workspace
git clone https://github.com/calebc42/ebp-poc.git ebp
git clone https://github.com/calebc42/grove-native.git bloom-native
for repo in ebp.el ebp-org ebp-kmp ebp-compose jetpacs-poc \
            jetpacs-authoring glasspane orgzly-native harp-native; do
  git clone "https://github.com/calebc42/$repo.git"
done
```

| Directory | What it holds |
|---|---|
| `bloom-native` | This repo: Bloom's screens and design presets, in Emacs Lisp |
| `jetpacs-poc` | The Jetpacs Companion app and the Emacs side of Jetpacs |
| `ebp` | The EBP specification and conformance files |
| `ebp.el` | EBP for Emacs: the connection, sync and storage |
| `ebp-org` | Org helpers shared by Jetpacs apps |
| `ebp-kmp` | EBP for Kotlin: the wire format and the Companion's engine |
| `ebp-compose` | The shared Compose renderer the Companion builds on |
| `jetpacs-authoring`, `glasspane`, `orgzly-native`, `harp-native` | Other Jetpacs apps; the Companion build bundles them |

If you keep the checkouts somewhere else, set `JETPACS_REPOSITORIES_ROOT` to
the directory that holds them.

## 2. Build and install the Companion

Point the build at your Android SDK, either with `ANDROID_HOME` or with
`sdk.dir` in `jetpacs-poc/companion/local.properties`. Then build the debug
app and install it:

```sh
cd jetpacs-poc/companion
./gradlew :app:assembleDebug
adb install -r app/build/outputs/apk/debug/app-debug.apk
```

The APK includes a copy of the Emacs side, Bloom Native among it. The next
step installs that copy into Android Emacs.

## 3. Connect Android Emacs

1. **Prepare Emacs:** install Emacs for Android and open it once. In Android
   settings, give Emacs **All files** access so it can read notes on shared
   storage.
2. **Start setup:** open the Companion and go to **Settings → Jetpacs Settings
   → Set up or repair Jetpacs**. Choose **Recommended** and tap **Continue**.
3. **Copy the setup block:** tap **Prepare files and copy setup**. The
   Companion copies a short setup block to the clipboard.
4. **Paste it:** in Android Emacs, open `~/.emacs.d/init.el` (`C-x C-f`),
   paste the block at the end, and save.
5. **Restart Emacs:** force-stop Android Emacs and open it again. On this
   start it installs Jetpacs under `~/.emacs.d/jetpacs/` and connects to the
   Companion.

After this, Emacs connects to the Companion each time Emacs starts. The
Companion shows **Emacs disconnected** whenever Emacs is not running.

Jetpacs's own [device onboarding guide](https://github.com/calebc42/jetpacs-poc/blob/main/docs/ONBOARDING-device.md)
covers the advanced setups, including Termux and a remote Emacs.

## 4. Turn on Bloom

1. **Use the Bloom look:** in the Companion, go to **Settings → Jetpacs
   Settings → Experiments** and turn on **Experimental Elisp design runtime**.
   Bloom's parity screens need it. Without it, Bloom draws plain Material
   screens.
2. **Install the app:** open the Companion's menu, choose **Apps**, and
   install **Bloom**. Bloom then appears in the menu.
3. **Pick a theme and your notes:** add these lines to
   `~/.emacs.d/jetpacs/user.el` in Android Emacs, then restart Emacs. That
   file loads last and survives Jetpacs updates.

   ```elisp
   ;; Bloom Light, the app's default palette. Also available:
   ;; "bloom.dark", "bloom.grove-light" and "bloom.grove-dark".
   (setq jetpacs-design-active-profile-id "bloom.light")

   ;; Bloom's notebooks are the files in Org's agenda scope.
   (setq org-directory "/sdcard/Notes/")
   (setq org-agenda-files '("/sdcard/Notes/" "/sdcard/Notes/projects/"))
   ```

Open Bloom from the Companion's menu. It starts on Notebooks.

## 5. Compare with the Android app

[`fixtures/parity-vault`](fixtures/parity-vault) holds the sample notebooks
used for the device comparisons. To see both apps side by side:

1. Copy the folder to the phone:

   ```sh
   adb push fixtures/parity-vault /sdcard/BloomParity
   ```

2. Point Bloom Native at that folder: set `org-directory` to
   `/sdcard/BloomParity/` and `org-agenda-files` to that folder and its
   `projects/` folder.
3. Open the same folder as the vault in the Bloom Android app.

The fixture's dates are set around October 23, 2026, so the Agenda's Today,
overdue and upcoming sections look different on other days.

## Develop

Bloom Native is plain Emacs Lisp under `elisp/`. Every module starts with
`bloom-`:
- `bloom-navigation.el` covers Notebooks and the Outline.
- `bloom-read.el` and `bloom-edit.el` cover reading and editing a note.
- `bloom-agenda.el` covers the Agenda.
- `bloom-design.el` holds the design presets: the colours, type scale and
  styles copied from the Android app.

[ELISP.md](ELISP.md) describes each workflow and its settings.

Run the tests from this directory. The runner runs the ERT suite and
Checkdoc, then byte-compiles every file with warnings as errors. It finds
`jetpacs-poc`, `ebp.el` and `ebp-org` as siblings.

```sh
./test-elisp/run-tests.sh
```

To try a change on the phone, rebuild and reinstall the Companion, which
bundles this directory. Then repeat the repair from step 3: **Prepare files
and copy setup**, replace the old block in `init.el` with the new one, and
restart Emacs.

## Troubleshooting

- **Emacs doesn't connect:** make sure only one Emacs is connected to the
  Companion. A desktop Emacs on the same pairing takes over the session.
  Each takeover drops the other side, and the "Emacs disconnected" banner
  flickers.
- **Jetpacs is missing after a restart:** pressing `C-g` while Emacs is still
  starting cancels `init.el` before Jetpacs loads. Restart Emacs and let it
  finish.
- **Bloom looks like plain Material:** check that the design runtime switch
  from step 4 is on and that `jetpacs-design-active-profile-id` names a Bloom
  preset.

## History

Bloom Native started as the Emacs Lisp half of Grove. It was split into its
own repository on 2026-09-05, keeping its commit history. When the Android
app was renamed from Grove to Bloom in October 2026, this project was renamed
in full to match:

- **Names:** the Elisp features are `bloom-*`, and the wire actions are
  `bloom.*`.
- **Surfaces and presets:** the surfaces are `app:bloom` and `widget:bloom.*`,
  and the design presets are `bloom.*`.
- **Old settings:** former `grove-*` Customize options still work as obsolete
  aliases.

The GitHub repository keeps its old name, `grove-native`.

## License

Apache License 2.0. See [LICENSE](LICENSE).
