set title "content-def"
set spec {
    stack
        { or
            { line
                /case
                { loop /name "/" }
                { loop { line (indent) INT : content-def (unindent)} {} }
            }
            { line /raw}
            { line /table { loop { line (indent) INT : \"STRING\" (unindent)} {} } }
            { line /string {or /ascii /icao } }
            { line
                { or /signed /unsigned }
                /integer
                { loop {} CONSTRAIN }
            }
            { stack
                { line
                    { or /signed /unsigned }
                    { line /quantity SCALE FRACT \"UNIT\" }
                    { loop {} CONSTRAIN }
                }
            }
        }
}

