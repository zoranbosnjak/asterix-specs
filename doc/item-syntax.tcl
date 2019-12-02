set title "item-def"
set spec {
    stack
        { or
            { stack
                { line { or
                    { line /subitems }
                    { line /compound }
                    { line /extended BITS-fst BITS-ext }
                    }
                }
                { line
                    { loop { line
                        (indent) NAME {''} TITLE {''}
                        { or {} {line /description TEXT } }
                        item-def (unindent) {}
                        }
                    }
                }
            }
            { line /repetitive item-def }
            { line /item BITS content-def }
            { line /spare BITS }
        }
}

