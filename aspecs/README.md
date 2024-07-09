# Asterix specifications conversion and validation tool

## Building and running

```bash
nix-build
./result/bin/aspecs -h
```

## Development

```bash
nix-shell

# fix permissions if necessary
chmod go-w .ghci
chmod go-w .

# lint
hlint {path}
find . | grep "\.hs$" | xargs hlint

# auto adjust style
stylish-haskell --inplace {path}
find . | grep "\.hs$" | xargs stylish-haskell --inplace

# run 'ghcid'
ghcid --no-title --lint "--command=ghci -Wall -iother -ilib -iapp app/Main.hs"

# run program, show usage
runhaskell -iother -ilib -iapp ./app/Main.hs --help

# convert spec to html (via pandoc)
runhaskell -iother -ilib -iapp ./app/Main.hs pandoc \
    --input-ast ../specs/test/test-cat-000-1.0-basic.ast | \
    pandoc -s -f native -t html -o output.html \
    --css=style.css --metadata title="asterix specification"
```
