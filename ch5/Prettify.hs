data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
  deriving (Show, Eq)

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

double :: Double -> Doc
double num = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

text :: String -> Doc
text str = undefined

string :: String -> Doc
string = enclose '"' '"' . hcat . map char

