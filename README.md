# horae

An application (read: hack) that runs Lisp scripts at certain
intervals.

Currently SBCL-specific.

Start it by calling `(horae:main)`.  Then you can create/modify/delete
scripts in the `horae/scripts/` directory at will, and horae will take
notice.

An alternative to deleting a script in order to disable it is to
rename it to something that begins with `ig-`.

Each script is loaded in the context of a temporary package that uses
`common-lisp`, as well as a horae-specific package that exports
symbols relevant to defining intervals.

All scripts run concurrently, each in its own thread.

Example simulation of a typical teenager:

```lisp
(declaim (interval 15 seconds))
(check-mobile-phone)
```

# License

MIT
