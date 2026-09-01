# Grove on Jetpacs

The `elisp/` tree is the start of Grove's replacement implementation as a
Jetpacs applet. It deliberately reuses Emacs Org instead of porting Grove's
Android parser, file store, index, capture engine, or mutation layer.

This first vertical slice includes:

- an Agenda-first `app:grove` surface backed by Jetpacs' canonical Org agenda
  extractor;
- Grove's sectioned ledger presentation and compact metadata rows;
- reveal-first, two-action swipes whose deeper commit marks the heading done
  through Org (the normal swipe only reveals controls);
- the existing Jetpacs Org capture flow as Grove's FAB and empty-state action;
- live `widget:grove.capture` and `widget:grove.agenda` surfaces for the one
  generic Room-backed Glance provider; and
- stable opaque row identities that can be resolved after Emacs process death
  without putting an Org path or reference on the wire.

The applet's builders only read the explicit snapshot in `grove-org.el`.
Agenda scanning and date bucketing run in the adapter producer after Org
changes. Android remains a generic renderer and durable event router.

Jetpacs stages these sources into its onboarding payload and lists Grove as an
optional packaged applet. Installing it is therefore an explicit user choice;
merely starting Jetpacs does not load Grove or scan Org.

Run the focused Elisp gates with:

```sh
./test-elisp/run-tests.sh
```

The Kotlin app remains the visual and workflow reference while screens are
ported. Its backend code is not a migration source because Emacs already owns
those capabilities.
