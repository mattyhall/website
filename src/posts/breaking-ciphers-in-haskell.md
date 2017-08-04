title: Breaking ciphers in haskell
date:  2013-03-16
tags: haskell, programming
description: Breaking a simple cipher by looking at the patterns of words
extends: default.liquid
---

Today I would like to share with you one of the first pieces of Haskell code I wrote that was more than ten lines long. It is a program to break a cipher.

**If you want to just look at the code, without the explain-y bit, scroll to the bottom or [click here](#listing)**

Ciphers are a method of encrypting text. For this blog post I will be looking at [substitution ciphers](http://en.wikipedia.org/wiki/Substitution_cipher) specifically. A substitution cipher is where each letter in the text that you want to encrypt is subsituted with another character -- the same letter, a different letter, a number etc. One of the most famous ones is ROT13, where each letter is substituted with the letter 13 places to the right, for example the letter 'A' is substituted with 'N'.

The normal method of breaking a cipher is by [frequency analysis](http://en.wikipedia.org/wiki/Frequency_analysis). When using this method the occurences of each character is counted; then each character is assigned a letter depending on how frequent the character was. The most frequent character in the cipher text is assigned the most frequent letter in English ("e"), the second-most frequent character the second-most frequent letter and so on. It is then decrypted and the text should be the original message.

However, I did not use this approach. Instead I used word-pattern analysis. This involves taking the first word from the ciphered text, for example "`qokkz`". The pattern of the word is then found; for the example it would be `ABCCD` as there is a `q` which becomes `A`, a `o` which become `B`, a `k` which becomes `C`, another `k` which we already know is `C` and then finally another unique character `z`, which becomes `D`. Next we have to look up all the words that have the same pattern. For each one we say that each letter corresponds to the same letter in ciphered word. This becomes our key, eg:

````text
qokkz (ABCCD) could be:
       hello (key: {q => h, o => e, l => k, z => o})
       cello (key: {q => c, o => e, l => k, z => o})
````

For each possible key we continuing the process -- getting the next word, get the pattern, find words that fit the pattern, find the words that also fit the key -- until there are no words left. Hopefully this will be clearer in code!

Both methods have there strengths. Frequency analysis can be used when there are no spaces in the text as it does not need to know where words begin and end, word-pattern analysis cannot. When I wrote this program frequency analysis seemed the more difficult option because it meant thinking of what to do when "``e``" was not the the most frequent (or "``a``" was not the second most frequent etc). Therefore I went with word-pattern analysis.

### The code
So the first step is to import some modules:

````haskell
import System.IO
import Data.List (nub, lookup, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
````

With that done we need a function that takes a word and returns the pattern:

````haskell
wordToPattern :: String -> String
wordToPattern xs = foldl (\acc x -> acc ++ [fromMaybe ' ' $ M.lookup x table]) "" xs
    where table = M.fromList $ zip (nub xs) ['a'..'z']
````
This function takes a string (`xs`) and folds over it. The annonymous function looks up the values of the character it is passed in the variable `table` and appends result to the accumalator (`acc`). So what is `table`? ``table`` uses the function `nub` to get all the unique characters in the string `xs` and puts each one in a tuple with a unique letter using the `zip` function. `zip` takes two lists and for each element in the first list returns a tuple with the element and the corrosponding element from the second list. For example, the word "hello" would become `[('h', 'a'), ('e', 'b'), ('l', 'c'), ('o', 'd')]`. It is then put in a map.

Next we need to get some real words from a dictionary and their patterns. To make things easier I used a map with the pattern as the key and a list of words with that pattern as the value:

````haskell
dictionaryPattern :: String -> M.Map String [String]
dictionaryPattern xs = foldl (\acc x -> M.insertWith (++) (wordToPattern x) [x] acc) M.empty $ words xs
````
Nothing too tricky going on here. We fold over all the words in `xs` (presumably from a dictionary) and get the pattern for each word. The word is then inserted into the map with the pattern as the key. If the key is not already in the map a list with the word is in, otherwise the word is added onto the end of the value using the function ``++``{.haskell}.

Also we could do with a function that unciphers some text using a key:

````haskell
uncipher :: String -> M.Map Char Char -> String
uncipher xs key = map (\x -> fromMaybe ' ' $ M.lookup x key) xs
````
The only thing of note here is that if the character is unknown (it is not in the key) then it becomes a space.

There is one last utility function to look at, ``wordFitsKey``:

````haskell
wordFitsKey :: String -> String -> M.Map Char Char -> Bool
wordFitsKey (x:xs) (y:ys) key
    | or [x  == y, and [x == ' ', y `notElem` (map snd $ M.assocs key)]] = wordFitsKey xs ys key
    | otherwise                                                          = False
wordFitsKey [] [] _ = True
````
This function takes two words and a key. It returns true if all the characters are either equal to each other or if the character in the first string is unknown (a space) and the character in the second string has not been discovered yet.

So now onto the meat of the program -- breaking the cipher. It will need to take a list of ciphered words, a map with actual words in and the key so far. Let's look at it a few lines at a time:

````haskell
breakCipher' :: [String] -> M.Map String [String] -> M.Map Char Char -> [M.Map Char Char]
breakCipher'  []     _    key = [key]
breakCipher' (x:xs) dict key = foldl' f [] wordsThatFit 
    where wordsThatFit = filter (\y -> wordFitsKey unciphered y key) $ fromMaybe [] 
                                                                     $ M.lookup (wordToPattern x) dict
          unciphered = uncipher x key
````
If there are no words left then the key is returned in a list. The key line, however, is the third. It filters a list of words that have the same pattern as the first word (`x`). It filters on whether the unciphered word; which for the first word would be a string with spaces in as there are no mappings in the key and so the ``fromMaybe`` call in ``uncipher`` would return a space for each character in the string as the ``M.lookup``{.haskell} will be ``Nothing``{.haskell}; fits the actual word. It then folds over each of the possible words with a function called ``f``. If there are no words that fit, due to the key being wrong, then an empty list is returned.

````haskell
f acc y = acc ++ breakCipher' xs dict (updateKey x y key)
````
We can see that all `f` does is call `breakCipher'` on the rest of the words with an updated key, given by the function ``updateKey``.

The ``updateKey`` takes three arguments: the ciphered word, the real word we are trying and the current key. Here it is:

````haskell
updateKey :: String -> String -> M.Map Char Char -> M.Map Char Char
updateKey (x':xs') (y:ys) key' = updateKey xs' ys $ M.insert x' y key'
updateKey []       []     key' = key'
````
It is another recursive function. Each call it takes the first characters from the what is left of the ciphered and real words. It then inserts into the key the ciphered character as the key and the real character as the value. It recurses until the two words are empty and then returns the update key.

``breakCipher'`` keeps recursing until there are no words left or there are no words found (the key is wrong). In the first instance the key is added onto the accumalator (``acc``) in `f` and in the second the accumalator is returned as concatenating a list with an empty list is the list. The empty list is returned by ``breakCipher'``.

<a id="listing"></a>

### Full code listing

````haskell
import System.IO
import Data.List (nub, lookup, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

dictionaryPattern :: String -> M.Map String [String]
dictionaryPattern xs = foldl (\acc x -> M.insertWith (++) (wordToPattern x) [x] acc) M.empty $ words xs
          
wordToPattern :: String -> String
wordToPattern xs = foldl (\acc x -> acc ++ [fromMaybe ' ' $ M.lookup x table]) "" xs
    where table = M.fromList $ zip (nub xs) ['a'..'z']

breakCipher xs dict = breakCipher' xs dict M.empty

breakCipher' :: [String] -> M.Map String [String] -> M.Map Char Char -> [M.Map Char Char]
breakCipher' (x:xs) dict key = foldl' f [] wordsThatFit 
    where wordsThatFit = filter (\y -> wordFitsKey unciphered y key) $ fromMaybe [] 
                                                                     $ M.lookup (wordToPattern x) dict
          unciphered = uncipher x key
          f acc y = acc ++ breakCipher' xs dict (updateKey x y key)
          updateKey (x':xs') (y:ys) key' = updateKey xs' ys $ M.insert x' y key'
          updateKey [] [] key' = key'
breakCipher' [] _ key = [key]

uncipher :: String -> M.Map Char Char -> String
uncipher xs dict = map (\x -> fromMaybe ' ' $ M.lookup x dict) xs

wordFitsKey :: String -> String -> M.Map Char Char -> Bool
wordFitsKey (x:xs) (y:ys) key
    | or [x  == y, and [x == ' ', y `notElem` (map snd $ M.assocs key)]] = wordFitsKey xs ys key
    | otherwise                                                          = False
wordFitsKey [] [] _ = True
````
