Syntax source files are in .tcl format.

On every change of the .tcl file(s), regenerate .ps files.
Generated .ps files shall be added to the repository too.

Example:

```bash
<edit tcl files>
nix-shell --run "./convertAll.sh"
git add *tcl *ps
```

