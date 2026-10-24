# Parity with Bloom 1.11

Bloom Native aims to look and behave like the Bloom Android app, version
1.11, screen for screen. "Matched" means a device screenshot of each app,
showing the same fixture vault, differs by at most a pixel or two. The
fixture vault on the test phone is `/sdcard/GroveParity`.

Glyph-level icon differences are accepted. Jetpacs draws Material icons where
the app draws its own glyphs, such as the back arrow and the menu icon.

## Matched

- **Notebooks:** the folder tree, notebook rows, monogram tiles, the dock and
  the drawer. Folders that hold planned notes open on first use.
- **Outline:** folding, the PREFACE band, keyword and priority chips, tags,
  planning chips and preview lines. Finished titles are struck through. Rows
  that wrap drift by about 1 px per extra line.
- **Read mode:** the title block, planning chips, drawers, blocks, lists with
  checkboxes, inline markup and the sub-heading fold carets.
- **Edit mode, keyboard down:** the raw Org field with the app's syntax
  colours, the save mark, autosave five seconds after the last edit, and the
  menu button.
- **Agenda, Today tab:** the header, the tab switch, the collapsed overdue
  card and the task rows.

## Built, not yet checked on the device

- **Agenda, Upcoming tab:** day sections such as "Tomorrow" and "Saturday,
  Oct 31", the repeat markers and the 14-day window.
- **Agenda, overdue card opened:** the overdue rows inside the card, with the
  "late" labels and deadline flags.
- **Agenda, options panel:** the grouping chips, the state filter and the
  three row toggles.
- **Agenda actions:** ticking a row done, "Move to today", and the row swipes.
- **Search:** the field row, the result count, the Advanced toggle and its
  operators panel, the Quick Start cards, saved searches, results grouped by
  notebook, and the Filters bar.

## Partly matched

- **Edit mode, keyboard up:** the app shows a suggestion strip, a smaller menu
  button and scroll-jump buttons. Bloom has none of these.
- **Edit mode, scrolling:** the app's 18 dp top gap scrolls away with the
  text. Bloom's stays fixed.
- **Edit menu button:** the app opens a metadata sheet for state, priority,
  tags and dates. Bloom opens its planning screen instead.
- **Agenda swipes:** the app offers a "Note" cell that logs a note into the
  LOGBOOK, and Bloom doesn't. The app also shows an undo snackbar after each
  change. Bloom shows a plain notice after "Move to today" only.
- **Search queries:** Bloom deliberately doesn't copy the app's query
  language. A Lisp form runs through org-ql when it's installed. Org match
  syntax runs through Org's matcher, and anything else through Org's text
  search. The four Quick Start cards are Org match queries, and they count
  the parity vault exactly as the app does.
- **Search filters:** the app's Filters sheet isn't built. Bloom's Filters
  button opens the operators panel instead.
- **Search as you type:** the app searches on every keystroke. Bloom searches
  when the keyboard's search key is pressed.
- **Search text matches:** the app also lists body-text snippets with the
  matched words highlighted. Bloom lists matching headings only.
- **org-ql text queries:** org-ql's text query syntax, such as `todo:TODO`,
  fails on Emacs 31 with an assertion inside its parser. Bloom passes only
  Lisp-form queries to org-ql.
- **Agenda title spacing:** the app tightens the day title by -0.21 sp.
  Jetpacs design dimensions can't be negative, so Bloom leaves it out.

## Not started

### Present in an older look

These work in Bloom Native today, but in Bloom's earlier Jetpacs-styled
look, not the app's.

- **Settings:** the app has a root page and fifteen sections. They are
  Agenda, Appearance, Backup, Capture templates, Notebooks, Notes, Reminders,
  Roam, Sharing, Sync, Tips, What's new, Widget, Developer and Bug report.
  Bloom has one settings screen. Whenever a setting is added or changed, the
  settings import and export must change with it.
- **Planning dates:** the shared screen that edits SCHEDULED and DEADLINE
  together. Bloom has its own planning calendar.
- **Capture:** the app has a template picker, a capture editor and a template
  editor. Bloom opens Jetpacs's Org capture dialog over Emacs's own capture
  templates.
- **Refile:** the app uses a refile sheet. Bloom offers refile from the
  Outline's long-press menu.
- **Home screen widgets:** Bloom's widgets still use the older look.

### No Bloom equivalent yet

- **Metadata sheet:** state, priority and tags, plus dates, Add note, Refile,
  Favorite and Generate org-id.
- **Sheets:** the state picker, the link picker and linked references.
- **Dailies:** the daily note screen and its date picker.
- **Read File:** a whole file in read mode.
- **Edit region:** editing part of a note on its own screen.
- **Onboarding** and the **What's new** dialog.
- **Sync screens:** the sync conflict screen and the sync log.
- **Reminder screens:** the reminder resolver and the exact-alarm prompt.

## Receiver gaps

These need Jetpacs or Companion work. Bloom can't fix them alone.

- **Tinted shadows:** the app tints the shadows of its floating button and
  sheets. Jetpacs has no shadow colour.
- **Navigation-bar scrim:** the app draws a white scrim behind the system
  navigation bar. Jetpacs doesn't.
- **Monospace spans:** monospace runs in rich text use the system monospace
  face, not Plex Mono.
- **Negative letter spacing:** design dimensions reject values below zero.
- **Scroll-aware insets:** an editor or list can't let its top padding scroll
  away with its content.
- **Undo snackbar:** a Bloom-authored snackbar with an Undo action needs a
  timed, dismissible receiver presentation.
