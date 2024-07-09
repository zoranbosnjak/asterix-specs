---
title: Specifications
---

# Asterix specifications

Content is generated from [repository](https://github.com/zoranbosnjak/asterix-specs),
revision `#$gitrev$`.

Category listing: [manifest.json](/manifest.json).

Revision: [gitrev.txt](/gitrev.txt)

Archive of all specifications:
    [[tar](/specs.tar)]
    [[tgz](/specs.tgz)]
    [[zip](/specs.zip)]

---

$for(nums)$
## category $num$

$if(hasCats)$
category editions:
$for(cats)$
* cat-$ed$
    [[ast](/specs/cat$n$/cats/cat$ed$/definition.ast)]
    [[txt](/specs/cat$n$/cats/cat$ed$/definition.txt)]
    [[json](/specs/cat$n$/cats/cat$ed$/definition.json)]
    [[pandoc](/specs/cat$n$/cats/cat$ed$/definition.pandoc.native)]
    [[pdf](/specs/cat$n$/cats/cat$ed$/definition.pdf)]
    [[html](/specs/cat$n$/cats/cat$ed$/definition.html)]
$endfor$
$endif$

$if(hasRefs)$
REF editions:
$for(refs)$
* ref-$ed$
    [[ast](/specs/cat$n$/refs/ref$ed$/definition.ast)]
    [[txt](/specs/cat$n$/refs/ref$ed$/definition.txt)]
    [[json](/specs/cat$n$/refs/ref$ed$/definition.json)]
    [[pandoc](/specs/cat$n$/refs/ref$ed$/definition.pandoc.native)]
    [[pdf](/specs/cat$n$/refs/ref$ed$/definition.pdf)]
    [[html](/specs/cat$n$/refs/ref$ed$/definition.html)]
$endfor$
$endif$

$sep$

---

$endfor$

---

Format description:

* *ast* source format  ([syntax description](/syntax.html))
* *txt* reformated source file, generated from ast
* *json* representation, generated from ast
* *pandoc* native documentation format
* *pdf* documentation, generated from pandoc
* *html* documentation, generated from pandoc
