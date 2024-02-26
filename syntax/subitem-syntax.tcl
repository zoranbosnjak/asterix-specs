set title "subitem-def"
set spec {
    stack
        { or
            { stack
                { line /group }
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
                { line { or
                    { line /extended }
                    { line /compound [n] }
                    }
                }
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
            { line /element BITS case-def(content-def) }
            { line /spare BITS }
        }
}

