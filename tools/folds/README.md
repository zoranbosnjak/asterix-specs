# Ad hoc specs analyses

Various "ad hoc" tools to check specs consistency or gather statistics.

Run from `tools` directory, for example

```bash
cd asterix-specs/tools
nix-shell
specs=$(find ../specs -type f | grep \.ast$)
runhaskell $EXTENSIONS -ilib -ifolds folds/units.hs ${specs}
runhaskell $EXTENSIONS -ilib -ifolds folds/numbers.hs ${specs}
```

