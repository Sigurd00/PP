import Control.Applicative
import Parsing

data Rexp = A | B | Conc Rexp Rexp | Union Rexp Rexp | Star Rexp | Par Rexp deriving Show

regexp = do
    s <- summand
    symbol "U"
    r <- regexp
    return (Union s r)
    <|>
    do
        s <- summand
        return s

summand = do
    f <- factor
    symbol "."
    s <- summand
    return (Conc f s)
    <|>
    do
        f <- factor
        return f

factor = do
    b <- base
    symbol "*"
    return (Star b)
    <|>
    do
        b <- base
        return b

base = do
    symbol "a"
    return A
    <|>
    do
        symbol "b"
        return B
    <|>
    do
        symbol "("
        r <- regexp
        symbol ")"
        return (Par r)

