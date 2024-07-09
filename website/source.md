---
title: Source
---

Sources of this project are available in github
[repository](https://github.com/zoranbosnjak/asterix-specs).

# Contributing

Contributions (problem reports, fixes) to this projects are welcome.

If you are a *github* user, the easiest way to contribute is via
[project's web page](https://github.com/zoranbosnjak/asterix-specs).
\
Feel free to contact project maintainer
[Zoran Bošnjak](mailto:zoran.bosnjak@sloveniacontrol.si)
via email.

## Contributing new categories

Before submitting new category, please make sure that
definitions are:

- complete (including remarks);
- validated with the [aspecs](/aspecs.html);
- prettified with the [aspecs](/aspecs.html);
- *code reviewed* by an independent reviewer;

# Project structure

- [specs/](https://github.com/zoranbosnjak/asterix-specs/tree/master/specs):
  Source files of [asterix definitions](/specs.html).

- [aspecs/](https://github.com/zoranbosnjak/asterix-specs/tree/master/aspecs):
  Implementation of [specs related tool](/aspecs.html).

- [to-pdf/](https://github.com/zoranbosnjak/asterix-specs/tree/master/to-pdf):
  PDF documentation generator, a simple wrapper
  around [pandoc](https://pandoc.org/) tool.

- [website/](https://github.com/zoranbosnjak/asterix-specs/tree/master/website):
  Implementation of this web site.

# Development

[nix](https://nixos.org/) package manager is required for development.
To install it, use:

```bash
curl -L https://nixos.org/nix/install | sh
```

... then logout and login again for changes to take effect.

Optionally install cachix client and use binary cache,
to speed up first build.

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use zoranbosnjak
```

## Clone project to local disk

```bash
git clone https://github.com/zoranbosnjak/asterix-specs.git
```

> **NOTE:** It might take some time for *nix* do download all required
dependencies for the first time or when switching to a new version of
nix packages.  Subsequent rebuilds on local changes are reasonably fast.

## Asterix category development

```bash
cd asterix-specs/specs/
nix-shell

# this project uses unix file format for specs files
# convert file from dos to unix if necessary
perl -pi -e 's/\r\n/\n/g' {file_name}

# validate selected file
aspecs validate -f {file_name} --ast --warnings

# prettify spec file
aspecs prettify --remove-comments {file_name} --ast

# build selected file
./build_spec.sh {file_name}

# check the result with some web browser
netsurf output/

# cleanup generated files when done
git clean -xdf output/

# validate all '.ast' files
for i in $(find . -type f | grep "\.ast$")
do
    aspecs validate -f $i --ast --warnings
    if [ $? -ne 0 ]; then echo $i; break; fi
done
```

## Complete project requild procedure

```bash
cd asterix-specs/
nix-build

# check locally generated web site with some web browser
netsurf ./result/index.html
```
