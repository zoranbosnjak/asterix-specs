Syntax source files are in .tcl format.

On every change of the .tcl file(s), regenerate .ps files.
Generated .ps files shall be added to the repository too.

Example (regenerate single):

```bash
<edit content-syntax.tcl>
nix-shell --run "./bubbleToPostscript.tcl < content-syntax.tcl > content-syntax.tcl.ps"
git add *tcl *ps
```

Example (regenerate all):

```bash
<edit tcl files>
nix-shell --run "./convertAll.sh"
git add *tcl *ps
```

Conversion from postscript to png, using `gs`.

```bash
gs -dSAFER -dBATCH -dNOPAUSE -dEPSCrop -r600 -sDEVICE=pngalpha -sOutputFile=outfile.png infile.ps
```

