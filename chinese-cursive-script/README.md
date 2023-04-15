# Pleco flashcards for "Chinese Cursive Script" by Fang-yü Wang

These are taken from "Read Chinese" (book one) by the same author. This book
introduces 300 characters, divided into 19 chapters:

* Chapter 1: 31 characters
* Chapter 2: 15 characters
* Chapter 3: 16 characters
* Chapter 4: 15 characters
* Chapter 5: 16 characters
* Chapter 6: 15 characters
* Chapter 7: 14 characters
* Chapter 8: 14 characters
* Chapter 9: 15 characters
* Chapter 10: 15 characters
* Chapter 11: 15 characters
* Chapter 12: 11 characters
* Chapter 13: 18 characters
* Chapter 14: 15 characters
* Chapter 15: 15 characters
* Chapter 16: 15 characters
* Chapter 17: 15 characters
* Chapter 18: 15 characters
* Chapter 19: 15 characters

The `.pleco` file is generated from the `.perlesson` file (which can be audited
against the character listing in the book):

```
cabal run genpleco -- \
  convert-per-chapter \
  --in ../chinese-cursive-script/chinese-cursive-script.perlesson \
  --out ../chinese-cursive-script/chinese-cursive-script.pleco \
  --category 'Chinese Cursive Script'
```
