module ParserPrinter
    exposing
        ( ParserPrinter
        , identity
        , compose
        , empty
        , alternative
        , map
        , path
        , query
        , parse
        , print
        )

{-|
Combines a parser and a printer. Essentially just a tuple under the hood.

# ParserPrinter
@docs ParserPrinter, parse, print

# Category
@docs identity, compose

# Alternative
@docs empty, alternative

# Iso Functor
@docs map

# Url
@docs path, query

-}

import Iso exposing (Iso)
import UrlSegment exposing (Segment)
import ParserPrinter.Parser as Parser
import ParserPrinter.Printer as Printer


-- PARSER/PRINTER


{-| A `ParserPrinter a b` takes an `a` to parse a `Segment` and results
in a `b` if parsing succeeds. It also takes a `b` to print a `Segment` and
returns a `a` if printing succeeds.
-}
type ParserPrinter a b
    = ParserPrinter
        { parser : Parser.Parser a b
        , printer : Printer.Printer a b
        }



-- CATEGORY


{-| The parser that succeeds without consuming any input.
-}
identity : ParserPrinter a a
identity =
    ParserPrinter
        { parser = Parser.identity
        , printer = Printer.identity
        }


{-| `compose q p` first runs `p`, then feeds its output to `q`.
-}
compose : ParserPrinter b c -> ParserPrinter a b -> ParserPrinter a c
compose (ParserPrinter p) (ParserPrinter q) =
    ParserPrinter
        { parser = Parser.compose p.parser q.parser
        , printer = Printer.compose p.printer q.printer
        }



-- ALTERNATIVE


{-| Parser that immediately fails.
-}
empty : ParserPrinter a b
empty =
    ParserPrinter
        { parser = Parser.empty
        , printer = Printer.empty
        }


{-| Try the first parser. If it fails, try the second (will backtrack appropriately).
-}
alternative : ParserPrinter a b -> ParserPrinter a b -> ParserPrinter a b
alternative (ParserPrinter p) (ParserPrinter q) =
    ParserPrinter
        { parser = Parser.alternative p.parser q.parser
        , printer = Printer.alternative p.printer q.printer
        }



-- ISO FUNCTOR


{-| Map the result of a parser. We need an isomorphism since we
use the forward direction for parsing and the backward direction
for printing. The isomorphism is partial, which means if it maps
to `Nothing` the parser will fail. For example, the parser

    map (Iso.iso (always Nothing) (always Nothing)) identity

is the same as `empty`.
-}
map : Iso a b -> ParserPrinter r a -> ParserPrinter r b
map iso (ParserPrinter p) =
    ParserPrinter
        { parser = Parser.map iso p.parser
        , printer = Printer.map iso p.printer
        }



-- URL


{-| Parse a component of the path.

    parse path segment
    -- /user ==> Just ("user", ())
    -- /user/ ==> Just ("user", ())
-}
path : ParserPrinter a ( String, a )
path =
    ParserPrinter
        { parser = Parser.path
        , printer = Printer.path
        }


{-| Parse a query parameter. We get back a list since a key
can appear any number of times in the query.

    parse query segment
    -- /?user="alex" ==> Just (["alex"], ())
    -- / ==> Just ([], ())
-}
query : String -> ParserPrinter a ( List String, a )
query key =
    ParserPrinter
        { parser = Parser.query key
        , printer = Printer.query key
        }



-- RUNNING


{-| Parse using a `ParserPrinter`. Returns the first match, if any.
-}
parse : ParserPrinter () a -> Segment -> Maybe a
parse (ParserPrinter p) =
    Parser.parse p.parser


{-| Print using a `ParserPrinter`. Returns the first valid output, if any.
-}
print : ParserPrinter () a -> a -> Maybe Segment
print (ParserPrinter p) =
    Printer.print p.printer
