Sqitch for Emacs
================

This package provides two modes for interacting with [sqitch](http://www.sqitch.org) projects.

# Installation

Ensure the files are on your load path, and require the modes:

``` el
(add-to-list 'load-path "/path/to/sqitch-for-emacs")
(require 'sqitch-plan-mode)
(require 'sqitch-mode)
```

Alternatively, if you favor
[el-get](https://github.com/dimitri/el-get), you can use this recipe:

```el
(:name sqitch
       :description "Emacs modes for interacting with Sqitch projects"
       :type github
       :pkgname "christophermaier/sqitch-for-emacs"
       :features (sqitch-mode sqitch-plan-mode))
```

# The Modes

## `sqitch-mode`

This is a minor mode for Sqitch `deploy`, `verify`, and `revert`
scripts. It allows you to easily jump between the different scripts
for a given changeset, as well as jump to the appropriate
`sqitch.plan` file.

Keybinding      | Description
----------------|---------------------
<kbd>C-c d</kbd>| Jump to corresponding "deploy" script
<kbd>C-c v</kbd>| Jump to corresponding "verify" script
<kbd>C-c r</kbd>| Jump to corresponding "revert" script
<kbd>C-c p</kbd>| Jump to `sqitch.plan` file

## `sqitch-plan-mode`

This is a major mode for `sqitch.plan` files. With point on a
changeset label (either at the beginning of a line or in a dependency
list), it allows you to jump directly to the `deploy`, `verify`, and
`revert` scripts for that changeset.

Keybinding      | Description
----------------|---------------------
<kbd>C-c d</kbd>| Jump to "deploy" script for changeset at point
<kbd>C-c v</kbd>| Jump to "verify" script for changeset at point
<kbd>C-c r</kbd>| Jump to "revert" script for changeset at point

## TODO

* Make changeset detection more robust; currently does not recognize
  tagged changesets as such.
