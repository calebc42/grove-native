# Grove on Jetpacs

The `elisp/` tree is Grove rebuilt as a Jetpacs applet. Emacs Org owns the
files, parsing, search, capture, TODO state, timestamps, and saves. Jetpacs
owns presentation transport, native Android rendering, durable offline action
delivery, and cached Glance widgets. Grove contributes the product's mobile
information architecture and visual workflows.

## Implemented slices

- A Notebooks-first `app:grove` shell with Notebooks, Agenda, Search, and
  Settings as primary adaptive destinations.
- A bounded nested notebook tree over the configured Org agenda scope, with a
  flat-list option, favorites, folder drill-down, and safe create, rename,
  move, and recoverable-trash operations.
- A collapsible native outline with preface and body previews, TODO metadata,
  heading navigation, Grove's reveal-first/deep-commit swipe behavior, and
  revision-bound insert/move/promote/demote/duplicate/refile/archive/delete
  operations. Copy/cut state, refile destinations, focus, and one-level undo
  remain private to Emacs.
- Grove-native read and edit modes. Read mode uses Jetpacs' neutral Org
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
- Grove presentation settings backed by Customize, plus persistent opaque
  notebook favorites.
- Timed Org reminders projected through Jetpacs' pairing-scoped Room store.
  Notification body taps open the opaque Grove document; ordered Complete and
  Reschedule actions enter the same durable EBP outbox. Complete invokes
  Emacs' configured done-state mutation, while Reschedule opens Grove's shared
  SCHEDULED/DEADLINE planning canvas. Android owns no Grove verb or Org path.
  The settings screen controls enablement and 0/15/60-minute or one-day lead
  time, and older peers receive the basic action-free reminder shape.
- The sectioned Agenda, Org capture FAB, richer multi-action swipe, and live
  `widget:grove.capture` / `widget:grove.agenda` surfaces from the first slice.

## State and safety boundaries

No Grove Android backend is used by the applet. In particular, it does not
port Grove's parser, JSON/Room repositories, FTS index, capture engine, sync
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

## Configuration

Grove treats `jetpacs-org-mode-agenda-scope` as its notebook and search scope.
Configure `org-directory`, `org-agenda-files`, and the corresponding
`ebp-org-roots` / `jetpacs-files-roots` in Emacs. Org roots authorize reads and
mutations; Files roots separately authorize notebook create, move, rename, and
trash operations.

The Grove Settings destination exposes presentation choices and conservative
bounds. The full variables remain ordinary Customize options:

- `grove-library-tree-view`
- `grove-library-notebook-limit`
- `grove-library-favorite-ids`
- `grove-reader-default-mode`
- `grove-reader-show-drawers`
- `grove-search-result-limit`
- `grove-saved-views`
- `grove-saved-view-limit`
- `grove-org-agenda-days`
- `grove-org-agenda-item-limit`
- `grove-org-planning-mark-limit`
- `grove-reminders-enabled`
- `grove-reminder-horizon-hours`
- `grove-reminder-lead-minutes`

## Packaging and verification

Jetpacs stages the complete `require` closure from these sources into its
onboarding payload and lists Grove as an optional packaged applet. Installing
it is an explicit user choice; merely starting Jetpacs does not activate Grove
or scan Org.

Run Grove's hermetic ERT, profile, Checkdoc, and warning-as-error byte-compile
gates with:

```sh
./test-elisp/run-tests.sh
```

The original Kotlin app remains the visual and workflow acceptance reference.
The next useful planning slice is structured time-range and repeater controls;
their timestamp semantics should continue to come from Emacs or reusable
Jetpacs platform seams.
