# Bloom on Jetpacs

The `elisp/` tree is Bloom rebuilt as a Jetpacs applet. Emacs Org owns the
files, parsing, search, capture, TODO state, timestamps, and saves. Jetpacs
owns presentation transport, native Android rendering, durable offline action
delivery, and cached Glance widgets. Bloom contributes the product's mobile
information architecture and visual workflows.

## Implemented slices

- A Notebooks-first `app:bloom` shell registered as a standalone Jetpacs app,
  like the Android app: no host bottom bar or global actions. Bloom draws its
  own drawer (Search, Notebooks, saved searches, Agenda, Settings, and a way
  back to the other Jetpacs apps) and its floating Search/Capture/New dock.
- Pixel parity with the Android app, screen by screen, under the design
  runtime (see "Pixel parity" below). Notebooks (tree, drawer, dock) and the
  Outline (folding, PREFACE band, chips, long-press structure menu) are done.
- A bounded nested notebook tree over the configured Org agenda scope, with a
  flat-list option, favorites, folder drill-down, and safe create, rename,
  move, and recoverable-trash operations.
- A collapsible native outline with preface and body previews, TODO metadata,
  heading navigation, Bloom's reveal-first/deep-commit swipe behavior, and
  revision-bound insert/move/promote/demote/duplicate/refile/archive/delete
  operations. Copy/cut state, refile destinations, focus, and one-level undo
  remain private to Emacs.
- Bloom-native read and edit modes. Read mode uses Jetpacs' neutral Org
  renderer under a surface-local link presenter; edit mode uses the shared Org
  toolbar and a stale-revision guard before a durable Emacs save.
- One scheduling canvas for SCHEDULED and DEADLINE, with an active-target
  switch, combined month calendar, date range, profile-gated endpoint colors,
  bounded date-only agenda marks, quick picks, lead-time feedback, and a
  deadline-derived suggestion. Both captured values are revision-checked and
  committed in one atomic Org mutation, including repeater cookies. Older
  month-grid member profiles receive colored mark fallbacks instead of
  `day_styles`.
- Native Org search: text/Boolean/brace-regexp queries use `org-search-view`,
  while structured tag, TODO, and property matches use `org-tags-view`.
  Invalid expressions keep the last valid result set visible.
- Hybrid saved views: searches authored on the phone persist as ordinary
  Customize data, while compatible built-in single commands from
  `org-agenda-custom-commands` appear read-only. Custom keys, matchers,
  settings, file names, and functions are never sent to Android.
- Bloom presentation settings backed by Customize, plus persistent opaque
  notebook favorites.
- Timed Org reminders projected through Jetpacs' pairing-scoped Room store.
  Notification body taps open the opaque Bloom document; ordered Complete and
  Reschedule actions enter the same durable EBP outbox. Complete invokes
  Emacs' configured done-state mutation, while Reschedule opens Bloom's shared
  SCHEDULED/DEADLINE planning canvas. Android owns no Bloom verb or Org path.
  The settings screen controls enablement and 0/15/60-minute or one-day lead
  time, and older peers receive the basic action-free reminder shape.
- The sectioned Agenda, Org capture FAB, richer multi-action swipe, and live
  `widget:bloom.capture` / `widget:bloom.agenda` surfaces from the first slice.

## State and safety boundaries

No Bloom Android backend is used by the applet. In particular, it does not
port Bloom's parser, JSON/Room repositories, FTS index, capture engine, sync
engine, or domain mutations. Android receives only neutral renderer nodes and
named action descriptors; it never implements “mark done”, “plan”, “capture”,
or file semantics.

Notebook paths, live buffers, and Org references remain in private Emacs maps.
Surface rows carry stable opaque hashes. After Emacs state loss those maps are
rebuilt from the bounded canonical Org scope before an action is resolved.
Android process death is handled independently by Jetpacs' Room surface cache
and durable action outbox.

File operations go through Jetpacs' reusable root-checked Files APIs. Creates
and destinations must be absent, all paths remain inside the configured Files
allowlist, symlinks cannot escape it, and deletion has no permanent fallback
when the host trash is unavailable. Device-originated prompts run only through
Jetpacs' dialog bridge outside the event dispatch extent.

Subtree saves compare the rendered SHA-256 revision with the live subtree,
restore the complete buffer if any pre-durability transform fails, and then use
the normal Org/Files save policy. Planning performs the same revision check
before its paired atomic mutation.

The planning canvas is the reference pilot for EBP's surface data document
(§13.8, capability `data.model`). Every value a picker tap changes — the two
timestamps, the dates and times behind the picker buttons and their labels,
the calendar month, its marks and day styles, the selection or range, the
target buttons' focus, and the feedback card's words — is bound to the
`/planning` object that `bloom-document-surface-data` builds from the same
draft state the screen builders read. A tap that changes values alone
patches that object through `jetpacs-app-defer-patch` (one `surface.data`
frame of under a kilobyte, the Companion re-materializing the screen it
holds); a tap that changes the shape of the tree — the feedback card
appearing or disappearing — re-pushes the stack as before. The calendar and
quick picks carry no target in their arguments: the focused target lives in
the draft, so the handler reads it there, and the runway suggestion is
computed when its button is tapped (`bloom.planning.suggest`) rather than
baked into the tree. A Companion without `data.model` receives the same
screens with the bindings resolved in Emacs.

## Configuration

Bloom treats `jetpacs-org-mode-agenda-scope` as its notebook and search scope.
Configure `org-directory`, `org-agenda-files`, and the corresponding
`ebp-org-roots` / `jetpacs-files-roots` in Emacs. Org roots authorize reads and
mutations; Files roots separately authorize notebook create, move, rename, and
trash operations.

The Bloom Settings destination exposes presentation choices and conservative
bounds. The full variables remain ordinary Customize options:

- `bloom-library-tree-view`
- `bloom-library-notebook-limit`
- `bloom-library-favorite-ids`
- `bloom-reader-default-mode`
- `bloom-reader-show-drawers`
- `bloom-search-result-limit`
- `bloom-saved-views`
- `bloom-saved-view-limit`
- `bloom-org-agenda-days`
- `bloom-org-agenda-item-limit`
- `bloom-org-planning-mark-limit`
- `bloom-reminders-enabled`
- `bloom-reminder-horizon-hours`
- `bloom-reminder-lead-minutes`

### Design profiles

When the Companion setting **Experimental Elisp design runtime** is enabled,
all seven Bloom app screens use a Bloom design preset. Bloom registers five:

- `bloom.default` ("Bloom") defines the IBM Plex type scale, spacing, shapes,
  state motion, and every component binding, but no fixed palette. Its color
  tokens resolve the active Jetpacs/EBP theme roles, including Modus and ef
  light/dark changes, and Bloom-only colors collapse onto the nearest role.
- `bloom.light` ("Bloom Light") and `bloom.dark` ("Bloom Dark") carry the
  Android app's default rosewater palettes verbatim, and `bloom.grove-light`
  ("Grove Light") and `bloom.grove-dark` ("Grove Dark") its original warm
  palettes, which the app still offers. Each includes the translucent soft
  fills and the eight Org syntax colors as literal color tokens. Like the
  app, they do not follow the system dark setting; the user picks one in
  Design Lab. All four re-declare the 13 EBP theme roles through their own
  tokens, so the
  Companion's chrome (scaffold, top bar, tabs, cards) takes the app's palette
  while they are active; `bloom.default` leaves chrome on the Emacs theme.

All presets share one type scale taken from the app's effective sizes and
bind the six canonical text slots, so every ordinary text node renders in
Plex Sans, Serif, or Mono by its EBP style name with no per-node wrappers.
The document screen nests a scope that rebinds only body text to the Serif
reading face, matching read mode. Buttons, chips, dividers, and section
headers stay ordinary canonical nodes; the receiver's design overrides render
them through the preset's `button.*`, `chip.*`, `divider.line`, and
`section-header.*` bindings. Widgets and notifications remain outside
this experiment.

Open **Jetpacs Design Lab** to inspect Bloom, save a complete editable copy,
and make that copy the device-wide active profile. The full snapshot is owned
and persisted by Emacs Customize; Android stores only cached surface documents.
Turning the runtime off restores Bloom's pre-experiment Material builders
without changing Bloom data, actions, identities, or Room schema. Companion
Settings and the setup/repair path remain receiver-owned and reachable while
Emacs is disconnected.

## Packaging and verification

Jetpacs stages the complete `require` closure from these sources into its
onboarding payload and lists Bloom as an optional packaged applet. Installing
it is an explicit user choice; merely starting Jetpacs does not activate Bloom
or scan Org.

Run Bloom's hermetic ERT, profile, Checkdoc, and warning-as-error byte-compile
gates with:

```sh
./test-elisp/run-tests.sh
```

The original Kotlin app remains the visual and workflow acceptance reference.
The next useful planning slice is structured time-range and repeater controls;
their timestamp semantics should continue to come from Emacs or reusable
Jetpacs platform seams.

## Pixel parity

The Android app is the acceptance reference, compared by screenshot on the
same device. When the receiver advertises `jetpacs.design` and the active
profile declares Bloom's parity faces (`bloom-design-parity-available-p`),
Bloom builds the app's own screens; otherwise the Material builders remain the
authority, so a portable profile copied before the faces existed never sends
a style the receiver cannot resolve.

Parity rests on a few measured facts:

- Every IBM Plex face is 1.3 em tall. The app's `Text` calls that name no
  style inherit Material's `bodyLarge`, which Bloom sets to 1.65 em; those
  that name a style keep 1.3 em. Each `bloom.t.*` text role records which.
- The receiver trims the extra leading above a first line and below a last;
  the app does not. Text roles restore it with a line-box `min_height` and
  half the extra as `padding_top` (the app centers it), and rows add the
  0.4 dp its whole-pixel line boxes accumulate.
- Monogram colors are Java's `String.hashCode` of the vault-relative path
  modulo the six palette keys; note counts are top-level headings plus one
  for a non-blank intro; ages follow Android's `DateUtils`.

Known receiver-side gaps: the drawer hamburger and the back arrow are the
Companion's Material icons rather than the app's `☰` and `←` glyphs (System
Back needs the `arrow_back` button); the app's thin custom icons and
accent-tinted shadows have no node equivalent; design text has no
strike-through for DONE titles; and the navigation-bar scrim stays clear.
Multi-line outline rows still drift about a pixel each from line rounding.

## Jetpacs outline

The Jetpacs applet keeps Bloom's notebook typography and theme while separating
browsing from reading. Parent headings show their workflow state and immediate
section count above the title. Tap the disclosure header to expand its children,
or choose **Read** to open the parent document. Leaf headings open directly.
Decorative heading arrows are omitted; only headings with children have a
functional disclosure control. Structure menus and swipe actions remain available.
