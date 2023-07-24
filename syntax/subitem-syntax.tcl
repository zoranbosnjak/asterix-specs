set title "subitem-def"
set spec {
    stack
        { or
            { stack
                { line { or
                    { line /group }
                    { line /extended
                        { or
                            {}
                            {/no-trailing-fx}
                        }
                        BITS-fst BITS-ext }
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
                { line /compound [n] }
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
            { line /repetitive { or /fx BITS} subitem-def }
            { line /explicit
                { or
                    {}
                    {/re}
                    {/sp}
                }
            }
            { line /rfs }
            { line /element BITS content-def }
            { line /spare BITS }
        }
}

