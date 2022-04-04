# Ad hoc specs analyses

Various "ad hoc" tools to check specs consistency or gather statistics.

Run from `tools` directory, for example

```bash
cd asterix-specs/tools
nix-shell
runhaskell -ilib -ifolds folds/units.hs $(find ../specs -type f | grep \.ast$)
```

