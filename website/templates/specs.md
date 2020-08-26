---
title: Specifications
---

# Asterix specifications

Content is generated from [repository](https://github.com/zoranbosnjak/asterix-specs),
revision `#$gitrev$`.

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
    [[xml](/specs/cat$n$/cats/cat$ed$/definition.xml)]
    [[rst](/specs/cat$n$/cats/cat$ed$/definition.rst)]
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
    [[xml](/specs/cat$n$/refs/ref$ed$/definition.xml)]
    [[rst](/specs/cat$n$/refs/ref$ed$/definition.rst)]
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
* *xml* representation, generated from ast (experimental)
* *rst* documentation format, generated from json
* *pdf* documentation, generated from rst
* *html* documentation, generated from rst

