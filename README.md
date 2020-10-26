# horae

An application (read: hack) that runs Lisp scripts between delays.

Scripts can run at fixed time intervals or as Poisson processes having
fixed mean time intervals.

Currently SBCL-specific.

Start it by calling `(horae:main)`.  Then you can create/modify/delete
scripts in the `horae/scripts/` directory at will, and horae will take
notice.

Each script is loaded in the context of a package that uses
`common-lisp`, as well as a horae-specific package that exports
symbols relevant to defining intervals.

The package is _not_ deleted when the script terminates, so when the
script is re-loaded, everything gets re-defined (of course you can use
`defvar` and such to prevent overriding values).

Note that after loading a script, the image will contain
script-related objects.  They will remain even if the script is
deleted.

An alternative to deleting a script in order to disable it is to
rename it to something that begins with `ig-`.

All scripts run concurrently, each in its own thread.

Example simulation of a typical teenager:

```lisp
(declaim (interval 15 seconds))
(check-mobile-phone)
```

# License

MIT
