set title "content-def"
set spec {
    stack
        { or
            { line
                /case
                { loop /item "/" }
                { loop { line (indent) INT : content-def (unindent)} {} }
            }
            { line /raw}
            { line /discrete { loop { line (indent) INT : STRING (unindent)} {} } }
            { line /string {or /ascii /icao } }
            { stack
                { line
                    { or /signed /unsigned }
                    { or {} { line /scale NUM} }
                    { or {} { line /fractional BITS } }
                }
                { line {opt /unit STRING } }
                { line
                    { or
                        {}
                        { line { or /ge /gt } NUM }
                    }
                    { or
                        {}
                        { line { or /le /lt} NUM }
                    }
                }
            }
        }
}

