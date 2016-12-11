module ParserPrinter exposing
  ( ParserPrinter
  , parse
  , print
  , map
  , product
  , alternative
  , empty
  , pure
  , path
  , query
  )
{-|

# Primitives
@docs ParserPrinter, parse, print, pure, path, query

# IsoFunctor
@docs map

# ProductFunctor
@docs product

# Alternative
@docs alternative, empty

-}


import Iso exposing (Iso)
import UrlSegment exposing (Segment)

import Parser
import Printer


-- PARSER

type ParserPrinter a =
  ParserPrinter
    { parser : Parser.Parser a
    , printer : Printer.Printer a
    }



-- ISO FUNCTOR

map : Iso a b -> ParserPrinter a -> ParserPrinter b
map iso (ParserPrinter p) =
  ParserPrinter
    { parser = Parser.map iso p.parser
    , printer = Printer.map iso p.printer
    }



-- PRODUCT FUNCTOR

product : ParserPrinter a -> ParserPrinter b -> ParserPrinter (a, b)
product (ParserPrinter p) (ParserPrinter q) =
  ParserPrinter
    { parser = Parser.product p.parser q.parser
    , printer = Printer.product p.printer q.printer
    }



-- ALTERNATIVE

alternative : ParserPrinter a -> ParserPrinter a -> ParserPrinter a
alternative (ParserPrinter p) (ParserPrinter q) =
  ParserPrinter
    { parser = Parser.alternative p.parser q.parser
    , printer = Printer.alternative p.printer q.printer
    }


empty : ParserPrinter a
empty =
  ParserPrinter
    { parser = Parser.empty
    , printer = Printer.empty
    }



-- SYNTAX

pure : a -> ParserPrinter a
pure x =
  ParserPrinter
    { parser = Parser.pure x
    , printer = Printer.pure x
    }


path : ParserPrinter String
path =
  ParserPrinter
    { parser = Parser.path
    , printer = Printer.path
    }


query : String -> ParserPrinter (List String)
query key =
  ParserPrinter
    { parser = Parser.query key
    , printer = Printer.query key
    }



-- RUN A PARSER/PRINTER

parse : ParserPrinter a -> Segment -> Maybe a
parse (ParserPrinter p) =
  Parser.parse p.parser

print : ParserPrinter a -> a -> Maybe Segment
print (ParserPrinter p) =
  Printer.print p.printer

