set title "subitem-def"
set spec {
    stack
        { or
            { stack
                { line { or
                    { line /subitems }
                    { line /extended BITS-fst BITS-ext }
                    }
                }
                { line
                    { loop { line
                        (indent) NAME {''} TITLE {''}
                        { or {} {line /description TEXT } }
                        subitem-def (unindent) {}
                        }
                    }
                }
            }
            { stack
                { line /compound }
                { line
                    { loop { line (indent)
                        { or -
                            { line NAME {''} TITLE {''}
                                { or {} {line /description TEXT } }
                                subitem-def
                            }
                        } (unindent) {} }
                    }
                }
            }
            { line /repetitive BYTES subitem-def }
            { line /explicit }
            { line /fixed BITS content-def }
            { line /spare BITS }
        }
}

