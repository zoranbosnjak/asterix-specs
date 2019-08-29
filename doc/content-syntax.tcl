set spec {
    stack
        { or
            { line /raw}
            { line /discrete { loop { line (indent) { or INT CHR HEX OCT } : STRING (unindent)} {} } }
            { line /string {or /ascii /icao } }
            { line /ssr}
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

