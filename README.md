# cl-double-metaphone

A pure Common Lisp implementation of **Lawrence Philips’ Double Metaphone** phonetic encoding algorithm.

Double Metaphone is a refinement of the classic Metaphone algorithm for English.  
It encodes words into short letter sequences that represent how they sound rather than how they’re spelled.  
This makes it useful for:

- Fuzzy text matching (e.g., “Smith” ≈ “Smyth”)
- Deduplication and record linkage
- Search engines and spell-checkers
- Linguistic or genealogy research

This implementation aims for **complete compatibility** with the reference C version, verified through a
comprehensive [FiveAM](https://github.com/sionescu/fiveam) test suite built from over a hundred English
homophone pairs.

---

## ✨ Features

- ✅ Returns **primary** and **secondary** phonetic keys for each word  
- ✅ Behavior verified against canonical Double Metaphone test data  
- ✅ Full [FiveAM](https://github.com/sionescu/fiveam) regression suite  
- ✅ No external dependencies—just ANSI Common Lisp  
- ✅ Licensed under Apache 2.0

---

## 📦 Installation

Clone or download the repository and load it with ASDF:

```lisp
(ql:quickload "cl-double-metaphone")
```

---

## 🧠 Usage

### 1. `double-metaphone`
Returns **two values** — the primary and secondary phonetic encodings for the given word.

```lisp
CL-USER> (multiple-value-list (double-metaphone:double-metaphone "Smith"))
("SM0" "XMT")

CL-USER> (multiple-value-list (double-metaphone:double-metaphone "Schmidt"))
("XMT" "SMT")
```

If two words sound alike, their primary or secondary keys will usually match.

---

### 2. `metaphone-primary`
Convenience wrapper that returns **only** the primary encoding.

```lisp
CL-USER> (double-metaphone:metaphone-primary "Knight")
"NT"
```

---

### 3. `metaphone-alternate`
Returns the **secondary (alternate)** encoding, when applicable.

```lisp
CL-USER> (double-metaphone:metaphone-alternate "Knight")
"NT"
```

For many English words, both encodings are identical.  
Differences appear for ambiguous spellings such as “Smith”/“Schmidt” or “Steven”/“Stephen”.

---

## 🧪 Running the Tests

```lisp
CL-USER> (ql:quickload "cl-double-metaphone/tests")
To load "cl-double-metaphone/tests":
  Load 1 ASDF system:
    cl-double-metaphone/tests
; Loading "cl-double-metaphone/tests"

("cl-double-metaphone/tests")
```

```lisp
CL-USER> (asdf:test-system :cl-double-metaphone/tests)

Running test suite DOUBLE-METAPHONE-SUITE
 Running test BASIC-ENGLISH-NAMES .....
 Running test SIMILAR-SOUNDING-NAMES ..
 Running test EDGE-CASES ....
 Running test CAPITALIZATION ..
 Running test ENGLISH-HOMOPHONES ........[...]
 Did 149 checks.
    Pass: 149 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```

---

## 📜 License

Apache 2.0

---

## 🤖 Disclaimer

This README was written with the assistance of ChatGPT.
