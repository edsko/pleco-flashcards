# Pleco flashcards for "Chinese Cursive Script" by Fang-yü Wang

These are taken from "Read Chinese" (book one) by the same author.

The `.pleco` file is generated from the `.perlesson` file (which can be audited
against the character listing in the book):

```
cabal run genpleco -- \
  convert-per-chapter \
  --in ../chinese-cursive-script/chinese-cursive-script.perlesson \
  --out ../chinese-cursive-script/chinese-cursive-script.pleco \
  --category 'Chinese Cursive Script'
```
