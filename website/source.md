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
- validated with the [tools](/tools.html) (enable `--warnings`);
- prettified with the [tools](/tools.html);
- *code reviewed* by an independent reviewer;

# Project structure

- [specs/](https://github.com/zoranbosnjak/asterix-specs/tree/master/specs):
  Source files of [asterix definitions](/specs.html).

- [tools/](https://github.com/zoranbosnjak/asterix-specs/tree/master/tools):
  Implementation of [specs related tools](/tools.html).

- [json-to-rst/](https://github.com/zoranbosnjak/asterix-specs/tree/master/json-to-rst):
  Asterix specification converter from `.json` to `.rst` documentation format.

- [rst-to-pdf/](https://github.com/zoranbosnjak/asterix-specs/tree/master/rst-to-pdf):
  PDF documentation generator, a simple wrapper
  around [pandoc](https://pandoc.org/) tool.

- [website/](https://github.com/zoranbosnjak/asterix-specs/tree/master/website):
  Implementation of this web site.

# Development

```bash
# install nix package manager
curl -L https://nixos.org/nix/install | sh

# clone repository
git clone https://github.com/zoranbosnjak/asterix-specs.git
cd asterix-specs/

# (re)build complete project
# It might take some time to compile everything for the first time,
# or when switching to a new version of nix packages.
# Subsequent rebuilds on local changes are reasonably fast.
nix-build

# show locally generated web site
firefox ./result/index.html
```

