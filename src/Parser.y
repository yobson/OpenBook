{
module Parser (parse, Entry(..)) where

import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
      title         { Title $$ }
      def           { Def $$ }
      eqn           { Eqn $$ }
      is            { Is $$ }
      body          { Body $$ }

%%

Entries : Entry Entries      { $1 : $2 }
        | Entry              { [$1] }

Entry  : Title Infos         { $1 : $2 }
       | Title               { [$1] }

Infos  : Info Infos          { $1 : $2 }
       | Info                { [$1] }

Info   : Def                 { $1 }
       | Eqn                 { $1 }
       | Is                  { $1 }

Bodies : body Bodies         { concat [$1, "\n", $2] }
       | body                { $1 }

Title  : title Bodies          { PETitle $1 $2 }

Def    : def Bodies            { PEDef $1 $2 }

Eqn    : eqn Bodies            { PEEqn $1 $2 }

Is     : is Bodies             { PEIs $1 $2 }

{

parseError :: [Token] -> a
parseError _ = error "Something went wrong!"

data Entry = Entry { eTitle :: (String,String)
                   , eDef :: [(String, String)]
                   , eEqn :: [(String, String)]
                   , eIs  :: [(String,String)]
                   , eRel :: [(String, String)]
                   } deriving (Show)

data PreEntry = PETitle String String
              | PEDef String String
              | PEEqn String String
              | PEIs  String String
              deriving (Show)

construct :: [PreEntry] -> Entry -> Entry
construct [] e                                 = e
construct ((PETitle s1 s2):xs) (Entry t d e i r) = construct xs (Entry (s1,s2) d e i r)
construct ((PEDef   s1 s2):xs) (Entry t d e i r) = construct xs (Entry t ((s1,s2):d) e i r)
construct ((PEEqn   s1 s2):xs) (Entry t d e i r) = construct xs (Entry t d ((s1,s2):e) i r)
construct ((PEIs    s1 s2):xs) (Entry t d e i r) = construct xs (Entry t d e ((s1,s2):i) r)

emp = Entry ("","") [] [] [] []

parse = map (flip construct emp) . parser
}
