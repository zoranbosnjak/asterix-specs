set spec {
    stack
        { or
            { line { or
                { line /subitems }
                { line /compound }
                { line /extended BITS-fst BITS-ext }
                }
                { loop { line
                    (indent) NAME {''} TITLE {''}
                    { or {} {line /description TEXT } }
                    item-def (unindent) {}
                    }
                }
            }
            { line /repetitive item-def }
            { line /item BITS content }
        }
}

