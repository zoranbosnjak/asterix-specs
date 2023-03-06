set title "uap-def"
set spec {
    stack
        { or
            { line /uap { loop { line (indent) { or name {-} } (unindent)} {} } }
            { line
                /uaps
                { stack
                    { line
                        /variations
                        { loop { line (indent) /uap-name { loop { line (indent) { or name {-} } (unindent)} {} } (unindent)} {} }
                    }
                    { line
                        { or
                            { line /case { loop /name "/" }
                              { loop { line (indent) INT : /uap-name (unindent)} {} }
                            }
                            {}
                        }
                    }
                }
            }
        }
}

