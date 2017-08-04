title: Merging multiple migrations into one statement
date: 22 Jul 2014 12:00:00 +0100
tags: haskell, programming
description: Merge multiple migrations into one statement using the Lens library in Haskell
extends: default.liquid
---

Yep, I'm back with more boringly named blog posts! In this post we'll take a ``CREATE TABLE`` statement and a list of ``ALTER TABLE`` statements and turn them into a single ``CREATE TABLE`` statement. We'll do so with a helping hand from Haskell, Attoparsec and Lens but first an explanation of migrations. Feel free to skip.

## Intro
For those of you familiar with web frameworks like Rails you will have heard of migrations. Most web apps have a database of some kind and as you're developing your website it is quite likely that you will need to make alterations to your tables. These are migrations - they change the schema of a table. They could be in the form of raw SQL statements or they could be in the form of a program with a special DSL for databases. This is the approach Rails uses.
 
This is great as you are developing but what about when you are deploying or when you decide to create the database from scratch? If each migration were to be run one after another it would be very slow and may be unreliable, due to the fact that a migration is basically a one time script which tend not to be maintained. What most systems do instead is to store the current schema and then load that into a frest database. As far as I can tell this could be done in one of two ways:

   1. After each migration is done ask the database for the current schema and store that.
   2. Get all the migrations that have been applied and the original statement which created the table and from this generate a single statement which will create the table in its correct form.

There are, as with everything in life, pros and cons to each. The first method requires you to have run all the migrations one after another at least once. This doesn't seem like too much of a problem but you would have to be careful not to misplace the file with the schema in, otherwise you have to run the migrations one after another and you have no longer solved the problem. You would also have to get the schema from the database and parse this information which may appear differently depending on what database engine you are using.

The main problem with the second approach is that it is not as likely to be correct. If you are getting the information straight from the horse's mouth - the working database - then, if your careful about parsing this information, you can be pretty sure you're right. Deducing the schema from the migrations is not as likely to be correct. You also have a problem in that some databases may have different syntax but this can be negated if you try and stick to syntax they can all interpret. There is a spec for SQL so you could just stick to that in your migrations.

I decided to have a go at building a program which followed the second method. I'm pretty sure that Rails and others use the first way but I thought that the second method was a more interesting problem to solve.

## Data types
So the first task will be to parse the SQL statements into something we can pattern match on. Let's define some Abstract Data Types (ADTs):

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Types where
import qualified Data.Text as T
import qualified Data.Map as M

data Type = CInt
          | CChar Int
          deriving (Show, Eq)

data Constraint = NotNull
                | PrimaryKey
                deriving (Show, Eq)
```

First up we have the column types. For simplicity I've just picked a couple of types from the [documentation of postgresql](http://www.postgresql.org/docs/8.4/static/datatype.html). The ``Char`` type stores an integer which says how many characters the column can hold. There is also an ADT for constraints. Constraints are conditions the value for a column must satisfy. Once again I have only picked a couple to show.

Also note the language pragma and imports I have included. TemplateHaskell will be used for the [lens library](https://hackage.haskell.org/package/lens). For those of you who don't know lenses are a nice way to access data inside ADTs and change it - something that can sometimes be difficult in Haskell. ``Data.Text`` is imported as a replacement for ``String`` and a ``Map`` (or dictionary if you're from the Python world) to link the names of columns to the type and constraints for that column.

```haskell
data Column = Column { _typ :: Type
                     , _constraints :: [Constraint]
                     } deriving (Show, Eq)

makeLenses ''Column
```

There's nothing too surprising in the definition of the ``Column`` type (a column has a type and some constraints, easy) except perhaps the lack of a column name. The reason for this is that we are going to store the columns in a ``Map`` where the name of the column is the key. We could have just said that a table contains a list of columns and stuck the name in the ``Column`` data type but this would mean you would have to search the list for the correct column when making alterations to it.

```haskell
data Statement = Create T.Text CreateStatement
               | Alter T.Text AlterStatement
               deriving (Show, Eq)

data AlterStatement = RenameTable T.Text
                    | RenameColumn T.Text T.Text
                    | AddColumn T.Text Column
                    | DropColumn T.Text
                    | ChangeColumnType T.Text Type
                    deriving (Show, Eq)

data CreateStatement = CreateTable { _cols :: M.Map T.Text Column
                                   } deriving (Show, Eq)

makeLenses ''CreateStatement
```

Finally we have the definitions of the statements themselves. First we say that a statement can either be a ``Create`` or an ``Alter``. The ``Text`` value for these is the table they are creating or acting on. There are five kind of alter statements which are fairly self explanatory; for all but ``RenameTable`` the first argument to the constructor is the name of the column.

Slightly more interestingly is ``CreateStatement``. This illustrates what I was talking about before with storing many columns as a map of the column's name to the column's type and contraints.

## Parsing SQL Statements
To parse the SQL I will be using the [Attoparsec](http://hackage.haskell.org/package/attoparsec) library. Another option would have been to use Parsec which may have given better error reporting as Attoparsec is aimed at efficiency. The libraries are quite similar though so it would be quite easy to change this code to use Parsec.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Control.Applicative
import Types
import qualified Data.Text as T
import qualified Data.Map as M
```

As usual first we have the imports. ``OverloadedStrings`` allows us to write Strings in the normal format:

```haskell
"Hello world"
```

But instead of having to be of type ``String`` they could be of type ``Text`` or ``ByteString``. This is sort of like if you write a number in Haskell it could be one of many types.

Note that ``takeWhile`` from Prelude is hidden as that name is also used by Attoparsec. We use the ``Text`` version of Attoparsec so we can parse ``Text``.

```haskell
parseTypeChar :: Parser Type
parseTypeChar = do
  asciiCI "CHAR"
  skipSpace
  char '('
  skipSpace
  n <- many1 digit
  skipSpace
  char ')'
  return $ CChar (read n)
```

The first thing to note is that all of the parser functions that will be shown here have the type ``Parser a``, in this case ``Parser Type``. The ``Parser`` type is provided by Attoparsec and allows us to parse ``Text``, stopping if there is an error in parsing. Basically this type looks after all the error handling and other bits and pieces for us. We just have to say what we want to parse.

It may help to know what ``parseTypeChar`` is trying to match. It will try to parse a string in the form ``CHAR(20)``. ``parseTypeChar`` starts by matching ``"CHAR"`` case-insensitively. It then matches an opening bracket with space either side. ``many1 digit`` matches 1 or more digits and returns them as a list which we call ``n``. In this case ``digit`` is a parser which matches a numeric character and returns it but ``many1`` works with any parser you give it, for example we could do:

```haskell
string :: Parser String

many1 (string "hi") :: Parser [String]
```

The function then matches a closing bracket, turns ``n`` into a number and returns it in a ``CChar``. The ``read`` is neccessary because ``digit`` returns the digit as a ``Char`` and ``many1`` gives us back a list. In effect ``n`` is actually a ``String`` so we must use ``read`` to make it into an ``Int``.

```haskell
parseType :: Parser Type
parseType = (asciiCI "INT" *> pure CInt) <|> parseTypeChar
```

``parseType`` shows parsing through the applicative interface. The ``<|>`` operator will attempt whatever is on it's left side and, if that fails, will try whatever is on it's right side. So in this case we are trying to either parse an int type or a char type. Parsing the int type is a further use of applicative - it uses ``*>`` which evaluate it's left argument and then evaluates it's right argument and returns the right. This can be a very useful tool when parsing because it allows you to do things like this:

```haskell
char '(' *> many1 digit <* char ')'
```

In this case it will match a pair of opening and closing brackets with digits in between, just like ``parseTypeChar``. There's no difference in functionality but it is a lot shorter. In addition note how the left part of ``parseType`` ends with a ``pure``. ``pure`` is the applicative version of ``return``. Because ``*>`` works on ``Parser``s we have to put any normal value we want into ``Parser``. This is what ``pure``/``return`` do.


```haskell
parseConstraint :: Parser Constraint
parseConstraint = (asciiCI "NOT" *> skipSpace *> asciiCI "NULL" *> pure NotNull) <|>
                  (asciiCI "PRIMARY" *> skipSpace *> asciiCI "KEY" *> pure PrimaryKey)

parseConstraints :: Parser [Constraint]
parseConstraints = many1 (parseConstraint <* skipSpace)
```

The constraints parser is quite similar. Perhaps note the fact that ``*>`` can be changed together easily and the use of ``many1`` again. 
```haskell
parseName :: Parser T.Text
parseName = takeWhile1 (`notElem` "; ")

parseColumn :: Parser (T.Text, Column)
parseColumn = do
  name <- parseName
  skipSpace
  t <- parseType
  skipSpace
  constraints <- parseConstraints <|> pure []
  return (name, Column t constraints)

parseCreate :: Parser Statement
parseCreate = do
  asciiCI "CREATE TABLE"
  skipSpace
  name <- parseName
  skipSpace
  char '('
  cols <- sepBy (skipSpace *> parseColumn <* skipSpace) (char ',') 
  skipSpace
  char ')'
  skipSpace
  char ';' <|> pure ' '
  return $ Create name (CreateTable (M.fromList cols))
```

Ah. Now we get to the more interesting bit! First we have ``parseName`` which is used for any name in a statement. In this case we just take any characters which aren't a space or a semicolon as either of those would end the name. ``parseColumn`` is pretty standard - we just grab a name, some spaces and a type. We then try to get some constraints. However, not all columns will have constraints. Once again we use ``<|>`` to allow for this. If ``parseCostraints`` fails then we'll just give back an empty list.

Finally we have a parser for a full statement! ``parseCreate`` is pretty straight forward, except for when we grab the columns. Here we use the ``sepBy`` parser which matches it's first argument and then it's second, repeatedly. In this case we want to have many columns each separated by a comma. In the last line of the function we call ``M.fromList``. Because parsing a column returns the name of the column and the column itself wrapped in a tuple, and because we get many columns, we can turn this into a ``Map``. This will make doing things like grabbing the column named "id" very easy and also efficient.

Statements do not have to end with a semicolon, depending on the database used, hence the use of ``<|>`` like in parseColumn. You may be wondering why we used ``pure ' '`` and not something like ``pure ()``. Although it wouldn't matter that much to us if we used the ``()`` version (as we're only going to throw it away anyway), ``<|>`` requires it's arguments to have the same type. As ``char`` has type ``Parser Char`` we must have the same type on the other side. 

Let's test it out in ghci:

```haskell
ghci> :l Parser.hs 
...
ghci> :set -XOverloadedStrings
ghci> parseOnly parseCreate "CREATE TABLE atbl (id INT PRIMARY KEY NOT NULL);"
Right (Create "atbl" (CreateTable {_cols = fromList [("id",Column {_typ = CInt, _constraints = [PrimaryKey,NotNull]})]}))
ghci> parseOnly parseCreate "CREATE TABLE atbl (id INT PRIMARY KEY NOT NULL, name CHAR(20));"
Right (Create "atbl" (CreateTable {_cols = fromList [("id",Column {_typ = CInt, _constraints = [PrimaryKey,NotNull]}),("name",Column {_typ = CChar 20, _constraints = []})]}))
```

Fantastic, it works! The only complicated bit was enabling ``OverloadedStrings``. As you can see the result is wrapped in a ``Right``. This is one of the constructors of the ``Either`` type which is used to handle parse errors. ``parseOnly`` has type ``Parser a -> Text -> Either String a`` so if the parser succeeds the value it parsed is returned in a ``Right``. If it fails then the error message is wrapped in a ``Left``. We can then use a case statement or pattern match on this value and handle the error.

Now we just have to parse the alter statements and the parser is done. Luckily it is not too complicated and, hopefully, you'll be able to understand it without any help from me. Let's deal with the parsing of the different types of alter statements first, eg. ``RENAME TO``:

```haskell
-- parses statements like:
-- RENAME TO name
parseRenameTable :: Parser AlterStatement
parseRenameTable = do
  asciiCI "RENAME TO"
  skipSpace
  name <- parseName
  return $ RenameTable name

-- RENAME colName TO newColName
parseRenameColumn :: Parser AlterStatement
parseRenameColumn = do
  asciiCI "RENAME"
  skipSpace
  oldName <- parseName
  skipSpace
  asciiCI "TO"
  skipSpace
  newName <- parseName
  return $ RenameColumn oldName newName

-- ADD col INT NOT NULL
parseAddColumn :: Parser AlterStatement
parseAddColumn = do
  asciiCI "ADD"
  skipSpace
  (name, column) <- parseColumn
  return $ AddColumn name column

-- DROP col
parseDropColumn :: Parser AlterStatement
parseDropColumn = do
  asciiCI "DROP"
  skipSpace
  name <- parseName
  return $ DropColumn name

-- col INT
parseChangeColumnType :: Parser AlterStatement
parseChangeColumnType = do
  name <- parseName
  skipSpace
  typ <- parseType
  return $ ChangeColumnType name typ
```

And now the last pieces of the puzzle - parsing an alter statement and a helper function to change text into a ``Statement``:

```haskell
parseAlter :: Parser Statement
parseAlter =  do
  asciiCI "ALTER TABLE"
  skipSpace
  name <- parseName
  skipSpace
  alter <- parseRenameTable <|> parseRenameColumn <|> parseAddColumn <|> parseDropColumn <|> parseChangeColumnType
  skipSpace
  char ';' <|> pure ' '
  return $ Alter name alter

parseStatement :: T.Text -> Statement
parseStatement xs = q
  where (Right q) = parseOnly (parseCreate <|> parseAlter) xs
```

``parseAlter`` should look fairly simple to you now. ``parseStatement`` is rather naughty and not good Haskell style. We use ``parseOnly`` like in the interactive ghci sessions but we grab the ``Statement`` value out of it by pattern matching. But what if the parser fails and we get a ``Left``? Well the program will crash - not good. Happily one of the benefits of writing a blog post is that you can only include statements that will work so I can probably get away with matching the ``Right``!

## Merging
### State
Now the good bit. That parsing was a bit of a slog but now we can actually have a go at merging statements into one. The approach I shall use is to have a ``Map`` of table names to ``CrateStatement``s. The advantages of using ``CreateStatement`` is two-fold:

  1. It allows us to store and edit all the table data - column names, types, constraints. You can think of ``CreateStatement`` as just a representation of a ``Table``.
  2. We can easily create a ``Statement`` out of it which we will return when we've merged all the migrations.

Hang on a second. "store and edit". It sounds like we needs some mutable state and so we're going to store this using the State monad. It provides us with three functions:

```haskell
-- State s a where s is the state held and a is the return type
get :: State s s
put :: s -> State s ()
modify :: (s -> s) -> State s ()
```

This is exactly the kind of thing we need. The problem is that modifying values can be a bit clunky. For example:

```haskell
-- Given the state M.fromList [('a', M.fromList [('1', 10)]), ('b', M.empty)]
change :: State (M.Map Char (M.Map Char Int)) () 
change = do
    m <- get
    let (Just a) = M.lookup 'a'
    -- inserting a value that already exists changed the associated value with the value given
    modify (M.insert 'a' (M.insert '1' 100 a))
```

As you can see when we have any kind of nesting then it gets a bit messy. Seeing as we will be storing data of the type ``M.Map T.Text CreateStatement``, where ``CreateStatement`` has extra data, we're going to need a nicer way to do this. We shall do this with lenses.

### Lenses
The [lens library](https://hackage.haskell.org/package/lens), written by Edward Kmett, provides a nice way to get and set values no matter how deeply nested they are. Lens can be quite difficult to use for beginners and I have to confess I am not exactly an expert in them myself. I do know just enough to accomplish what we are doing today though.

Let's quickly run through how to use lens. We'll create a data type for a description and a point:

```haskell
data Description = Description { _terrainType :: String
                               , _town :: String}
makeLenses ''Description

data Point = Point { _x :: Int
                   , _y :: Int
                   , _z :: Int
                   , _description :: Description
                   }
makeLenses ''Point
```

The function ``makeLenses`` automatically generates lenses for the data type given, so each accessor (``_x``, ``_description`` etc) will get a lens (``x``, ``description`` etc). We used this when declaring ``Column`` and ``Statement`` earlier. One way to think of lenses is as a path to data - a reference. ``x`` is a reference from a ``Point`` to an ``Int``. The type signature for lenses almost reads like the explanation:

```haskell
x :: Lens'            Point        Int
--    ^                 ^           ^
-- this is a lens   from a Point  to an Int
```

We can now use them like so:

```haskell
ghci> let p = Point 10 10 10 (Description "Sandy" "Eastbourne")
ghci> view x p
10
ghci> view (description . terrainType) p
"Sandy"
ghci> p ^. description . terrain
"Sandy"
```

``view`` is a function which gives back the value pointed to by the lens (given as the first argument) in the second argument. Notice how we can compose lenses with ``.`` - function composition. It even reads like an OOP accessor would. Having to wrap the lens in brackets is a bit tiresome though, so there is a nice operator defined (``^.``) which is just ``view`` renamed.

We can also set values:

```haskell
ghci> set x 0 p
Point 0 10 10 (Description "Sandy" "Eastbourne")
ghci> set (description . terrainType) "Muddy" p
Point 10 10 10 (Description "Muddy" "Eastbourne")
```

You may be wondering why this is of use to us. Sure, it's nice that we can get and set the data so easily but we're still going to have to get it and put it from the ``State`` monad, right? Luckily we don't need to! Let's rewrite that nested ``Map`` example:

```haskell
-- Given the state M.fromList [('a', M.fromList [('1', 10)]), ('b', M.empty)]
change :: State (M.Map Char (M.Map Char Int)) () 
change = at 'a' . _Just . at '1' .= Just 100
```

Wow, that's a lot less code! It's pretty dense though. Let's break it into chunks. 

  * ``.=`` is like ``set`` only it works on the state in the ``State`` monad. Because it's working on the state we don't need to pass it the data structure we want to change
  * ``at 'a'``. The lens ``at`` accesses the value in containers like ``Map``. Here we get the value associated with ``'a'``. The problem with ``Map`` structures is that no value may exist for that key. For this reason ``at`` returns the value associated with the key given wrapped in a ``Maybe``.
  * ``_Just . at '1'``. Before we can access the ``Map`` within the ``Map`` we have to unwrap it from it's ``Maybe`` coating. This is the reason for the ``_Just``. We can then access the value at the key ``'1'``.
  * ``Just 100``. Remember how ``at`` wraps the value it points at in a ``Maybe``? Well that means we have to set it to a value which is a ``Maybe``. In this case we use ``Just 100``. But what if we had used ``Nothing``? In that case the key and it's value is deleted from the ``Map``, if it existed. Nifty!

Hopefully this has piqued your interest in Lens. I have been purposefully brief as I didn't want to fall into the trap of writing a lens tutorial; I predict that lens is the new monad! Fortunately there are some great resources out there if. Take a look at [Program imperatively using Haskell lenses](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html) and [A Little Lens Starter Tutorial](https://www.fpcomplete.com/user/tel/a-little-lens-starter-tutorial).

### Merging statements
So to recap we shall have a list of ``Statement``s which will be given to a function. Each of these ``Statement``s will be passed to a function which will update it's state to reflect the changes the migration asked for.

```haskell
module Sql where

import Types
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import qualified Data.Text as T

type Merger a = State (M.Map T.Text CreateStatement) a

mergeMigration :: Statement -> Merger ()

singleStatementFromMigrations :: [Statement] -> [Statement]
singleStatementFromMigrations statements = M.foldWithKey (\tblName cStatement acc -> Create tblName cStatement : acc) [] m
    where m = execState (mapM_ mergeMigration statements) M.empty
```

Here's our entry point. For each statement we apply a function called ``mergeMigration`` using ``mapM_``. This is vary similar to the ``map`` function for lists only ``mapM_`` applies a monadic function, in this case a function with state, to each value in the list and then throws away all the values that were returned. It's important to note that the state will be retained from the last call, so if the first run of ``mergeMigration`` changes the state then this change will still be there on the second run.

We then use ``execState`` to run the stateful action which ``mapM_`` gave us with the default state of an empty ``Map`` and get back the final state (of type ``M.Map T.Text CreateStatement``). We then fold through the map, building up a list of ``Statement``s from the key of the ``Map`` (which is the table name) and the value (which is the ``CreateStatement``). In short we'll have one create statement for each table that was created and each statement will reflect any changes made with alter statements!

So let's have a look at the first few bits of ``mergeMigration`` then:

```haskell
mergeMigration :: Statement -> Merger ()
mergeMigration (Create name ctbl) = at name .= Just ctbl  

mergeMigration (Alter name (RenameTable newName)) = do
    tbl <- use $ at name
    at name .= Nothing
    at newName .= tbl
```

The first, and easiest, case is if we get a create statement. In this case we grab the name of the table and the ``CreateStatement`` which holds the information about the columns and associate the name with the ``CreateStatement`` in the ``Map``. Remember we store the ``CreateStatement`` so we can build a ``Statement`` easily and because it already has all the data we need.

The next case is a bit more interesting. If we need to rename a table then we get the data we currently have on that table using the ``use`` function. You can think of ``use`` as ``view`` but in the state monad. We then remove the table from the ``Map`` by using ``at`` and setting it's value to ``Nothing`` as seen before. Finally we reinsert the table at the key ``newName``.

Both of these cases and all of the cases I shall show could have quite easily been written without ``State``. The second one would look a little like this:

```haskell
mergeMigration (Alter name (RenameTable newName)) m = m''
    where tbl = m ^. at name
          m'  = set (at name) Nothing m 
          m'' = set (at newName) tbl m'
```

The state version looks easier to read to me but the advantage is minimal elsewhere. At any rate, we've saved ourselves some typing and what is more important that that? Certainly not readability.

We've now got something that we can test, so fire up ghci:

```haskell
ghci> :set -XOverloadedStrings
ghci> import Parser
ghci> import Sql
ghci> let stmts = map parseStatements ["CREATE TABLE tbl (col1 INT);", "ALTER TABLE tbl RENAME TO atbl"]
ghci> singleStatementFromMigrations stmnts
[Create "atbl" (CreateTable (M.fromList [("col1", Column Int [])]))]
```

Magic! I'm available to book for birthday parties. :D

### Error handling
So it works for correct input. But what about for incorrect input, for example altering a table that does not exist? In that case the magic runs out a bit:

```haskell
ghci> let stmts = map parseStatements ["ALTER TABLE tbl RENAME TO atbl"]
ghci> singleStatementFromMigrations stmnts
[]
```

Uh-oh. Returning an empty list isn't too bad but really we should be returning an error of some kind here. Altering a table that doesn't exist is not good behaviour and failing quietly is generally a bad idea. Let's fix this!

In Haskell there are two basic ways to handle errors. The first is the ``Maybe`` type which we have already seen. A value can either be ``Just a`` indicating, a successful result of ``a``, or ``Nothing``, idicating a unsuccessful result. Although that is a bit better than what we have, it would be nice to be able to give some extra information about the error. This is where the ``Either`` type comes in, which has two type paramaters (ie ``Either a b``). Hopefully you remember this from ``parseOnly``. ``Either`` has two constructors:

  * ``Left a``. Conventionally used to signify an unsuccessful result with information ``a``
  * ``Right b``. A successful result of ``b``

I say conventionally because ``Either`` can be used for other things as well. There's one more piece of the puzzle left however. For our error handling to work we could do with stopping at the first error we encounter. Luckily ``Either`` can do this as well. Just like ``Maybe`` we can use ``Either`` as a monad. Consider the following:

```haskell
test :: Either String Int
test = do
  Right 10
  Left "Error"
  Right 20

ghci> test
Left "Error"
```

Excellent! It turns out if we use ``Either`` as a monad it will stop and return (although as this is Haskell that may be a misuse of the term 'return') the first ``Left`` it finds. But how do we use this with the State monad? The more expirienced Haskellers amongst you may already know the answer - monad transformers.

A monad transformer allows us to make one monad out of multiple other monads and the one monad shares the properties of the ones it was made out of. In our case it will allow us to pass state around and do some error handling. Let's redefine ``Merger``:

```haskell
type Merger a = StateT (M.Map String CreateStatement) (Either String) a
--                           ^                              ^         ^
--                          state                      inner monad  return type
```

As you can tell ``StateT`` takes some state, a monad and a return type. You may be wondering why the inner monad is ``Either String`` and not just ``Either``. A basic explanation is to create a monad your type must have a space for the return value ``a``. If we just tried using ``Either`` then we have two spaces and this wouldn't work. Admittedly this is a pretty bad explanation - it is only half correct and a little simplistic. Luckily you do not have to understand completely to be able to follow along.

Let's rewrite the first couple of cases of mergeMigration:

```haskell
mergeMigration :: Statement -> Merger ()
mergeMigration (Create name ctbl) = do
    n <- use $ at name
    case n of 
        Just _ -> lift $ Left ("Table '" ++ T.unpack name ++ "' already created")
        Nothing -> at name .= Just ctbl  

mergeMigration (Alter name (RenameTable newName)) = do
  tbl <- use $ at name                 
  case tbl of
    Just _  -> do at name .= Nothing
                  at newName .= tbl
    Nothing -> lift $ Left ("Table '" ++ T.unpack name ++ "' does not exist so cannot rename")
```

In the first case we grab the ``CreateStatement`` associated with ``name``. Because ``at`` will give us back a ``Maybe`` when we get data with it we can use a case statement. If there was no ``name`` in the ``Map`` then we can insert it as normal. If there was a value associated with that key then we create an error message in a ``Left``. We then pass this ``lift``. ``lift`` is a special function that lifts functions of the inner monad into the outer one. In this program it will take our ``Either String a`` and turn it into ``StateT (M.Map String CreateStatement) (Either String) a``. Now to check if it works:

```haskell
ghci> let stmts = map parseStatements ["ALTER TABLE tbl RENAME TO atbl"]
ghci> singleStatementFromMigrations stmnts
Left "Table 'tbl' does not exist so cannot rename"
ghci> let stmts = map parseStatements ["CREATE TABLE tbl (col1 INT);", "ALTER TABLE tbl RENAME TO atbl"]
ghci> singleStatementFromMigrations stmnts
Right [Create "atbl" (CreateTable (M.fromList [("col1", Column Int [])]))]
```

Error reporting works and we haven't messed anything else up.

### Last ``mergeMigration`` cases
We only have four cases left!

```haskell
mergeMigration (Alter name (RenameColumn oldCol newCol)) = do
  m <- get
  let col = m ^? at name . _Just . cols . at oldCol . _Just
  case col of
    Just _ -> do at name . _Just . cols . at oldCol .= Nothing
                 at name . _Just . cols . at newCol .= col
    Nothing -> lift $ Left ("Column '" ++ T.unpack oldCol ++ "' does not exist in table '" ++ T.unpack name 
                            ++ "' so cannot rename")

mergeMigration (Alter name (AddColumn colName col)) = do
    m <- get
    let c = m ^? at name . _Just . cols . at colName . _Just
    case c of
        Just _ -> lift $ Left ("Column '" ++ T.unpack colName ++ "' already exists in table '" ++ T.unpack name 
                               ++ "' so cannot add")
        Nothing -> at name . _Just . cols . at colName .= Just col

mergeMigration (Alter name (DropColumn colName)) = do
    m <- get
    let c = m ^? at name . _Just . cols . at colName . _Just
    case c of 
        Just _ -> at name . _Just . cols . at colName .= Nothing
        Nothing -> lift $ Left ("Column '" ++ T.unpack colName ++ "' does not exist in table '" ++ T.unpack name 
                                ++ "' so cannot drop")

mergeMigration (Alter name (ChangeColumnType colName t)) = do
    m <- get
    let c = m ^? at name . _Just . cols . at colName . _Just
    case c of 
        Just _ -> at name . _Just . cols . at colName . _Just . typ .= t
        Nothing -> lift $ Left ("Column '" ++ T.unpack colName ++ "' does not exist in table '" ++ T.unpack name 
                                ++ "' so cannot change type")
```

What's this weird ``^?`` thing? It looks similar to ``^.`` and it does actually do something very similar. It turns out ``_Just`` can have 0 or 1 targets which is a subset of having 0 or more targets. In lens the former is called a ``Prism`` and the latter a ``Traversal``. If we used ``^.`` then we would have to combine all the results in some way. Lens does this with monoid. Unfortunately it doesn't really make sense to have a monoid instance for any of our types and because we're actually using a prism we only care about one result. ``^?`` grabs the first result only and puts it in a ``Maybe``. There is also ``^..`` which would give you all the results in a list. This is kind of where my knowledge of lens runs out but there are much better explanations of lens at the links listed previously and some good information on prisms and traversals in [this tutorial about lenses and Data.Aeson - a JSON library](https://www.fpcomplete.com/user/tel/lens-aeson-traversals-prisms).

Let's make sure all of this works:

```haskell
ghci> let stmntTexts = [ "CREATE TABLE tbl (col1 INT NOT NULL, col2 CHAR(20))"
                       , "ALTER TABLE tbl RENAME TO atbl"
                       , "ALTER TABLE atbl RENAME col1 TO first_col;"
                       , "ALTER TABLE atbl first_col CHAR(15)"
                       , "ALTER TABLE atbl ADD col3 INT;"
                       , "ALTER TABLE atbl ADD tmp INT;"
                       , "ALTER TABLE atbl DROP tmp;"]
ghci> print $ singleStatementFromMigrations stmnts
Right [Create "atbl" (CreateTable {_cols = fromList [("col2",Column {_typ = CChar 20, _constraints = []}),("col3",Column {_typ = CInt, _constraints = []}),("first_col",Column {_typ = CChar 15, _constraints = [NotNull]})]})]
```

Great, it works! Pretty printing the statements is left as an exercise to the reader - I've already rambled on enough!

And that's it. The code is a bit more repetative then I would like, I considered adding a function that would remove all the ``case`` statements:

```haskell
maybeM :: (a -> Merger ()) -> Merger () -> Merger (Maybe a) -> Merger ()
maybeM success failure mMaybe = do
    m <- mMaybe
    maybe failure success m
```

The first argument is a function which is run if the maybe returned a ``Just`` value. The second argument is the action to run if it was ``Nothing`` and finally there is the maybe. It would have allowed us to write code like this:

```haskell
mergeMigration :: Statement -> Merger ()
mergeMigration (Create name ctbl) = maybeM err (at name .= Just ctbl) (use $ at name)
    where err _ = lift $ Left ("Table '" ++ T.unpack name ++ "' already created")

mergeMigration (Alter name (RenameColumn oldCol newCol)) = maybeM f err (get >>= \m -> return (m ^? at name . _Just 
                                                                                    . cols . at oldCol . _Just))
  where err   = lift $ Left ("Column '" ++ T.unpack oldCol ++ "' does not exist in table '" 
                                        ++ T.unpack name ++ "' so cannot rename")
        f col = do at name . _Just . cols . at oldCol .= Nothing
                   at name . _Just . cols . at newCol .= Just col
```

The first one is still quite readable I think but the second is a little hairy. Higher order functions can be very useful and make code brief but sometimes readability should triumph over brevity. I'm still on the fence about whether it is an improvement or not.
