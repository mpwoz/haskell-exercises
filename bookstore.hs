-- This file doesn't work, just typing notes from chapter 3
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)


myInfo = Book 978439834289 "Algebra" ["Richard Bird", "Oege de Moor"]


type CustomerID = Int
type ReviewBody = String

type BookRecord = (BookInfo, BookReview)

data BookReview = BookReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
