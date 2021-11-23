#lang racket
(require csc151)
(require csc151/rex)
(require rackunit)
(require rackunit/text-ui)

;; language.rkt
;
;; Language Generation 
;
;; CSC-151 Fall 2021
;; Mini Project 5
;; Author: Quang Le
;; Date: 2021-10-28
;; Acknowledgements:
;; Professor Sam Rebelsky
;; Professor Jimenez
;; Classmate Anh Vu
;; Mini Project 3 
;; http://www.yougowords.com/3-syllables
;; https://www.madlibs.com/wp-content/uploads/2016/04/VacationFun_ML_2009_pg15.pdf
;; http://www.ashley-bovan.co.uk/words/partsofspeech.html
;; https://www.usna.edu/Users/cs/roche/courses/s15si335/proj1/files.php%3Ff=names.txt.html
;; https://github.com/aaronbassett/Pass-phrase/blob/master/verbs.txt

;; +------------------------------------------------------------------------
;; | Sample Syllaxes |
;; +------------------

(define csc151-syllax
  (list
   ; 0
   (list)
   ; 1
   (list "cons" "car" "list" "pair" "Scheme" "sort" "match" "string"
         "lab" "map" "fold" "test") 
   ; 2
   (list "cadr" "cdr" "Racket" "jelly" "sandwich" "syllax" 
         "image" "recurse" "eboard" "data" "compose" "lambda" "section"
         "SoLA" "MP")
   ; 3
   (list "recursion" "computer" "digital" "confusing" "programming" 
         "CSC" "abstraction" "decompose" "document" "abstraction"
         "boolean" "binary")
   ; 4
   (list "humanities" "exponential" "collaborate" "one-fifty-one"
         "algorithm" "DrRacket" "dictionary" "generalize" "defenestrate")
   ; 5
   (list "collaborative" "experiential" "decomposition" "generality"
         "defenestration")
   ; 6
   (list)
   ; 7
   (list "triskaidekaphobia")))

(define grinnell-syllax
  (list
   ; 0
   (list)
   ; 1
   (list "Mears" "Noyce" "husk" "train" "corn" "black" "HSSC")
   ; 2
   (list "self-gov" "Stonewall" "The Bear" "first-year" "scarlet"
         "remote" "Webex" "prairie" "need-blind" "soybeans" "Hopkins"
         "Younker" "Dibble" "cluster" "scurry" "Gremlin")
   ; 3
   (list "liberal" "JRC" "CLS" "advisor" "FYE" "laurel leaf" "Honor G"
         "ARH" "North Campus" "Iowa" "semester" "Women's quad"
         "Grinnellian" "pie-on-ears")
   ; 4
   (list "curriculum" "Mary B. James" "Tutorial" "Green alien" 
         "convocation" "education")
   ; 5
   (list)
   ; 6
   (list "Congregationalist")))

;; +--------------------------------------------------------------------
;; | My Syllax | 
;; +------------

(define my-syllax
  (list
   ; 0
   (list)
   ; 1
   (list "car" "train" "bus" "love" "hug"
         "pass" "fail" "you" "me" "world") 
   ; 2
   (list "purple" "perfect" "airplane" "seven" "again"
         "thursday" "pizza" "thursty" "water" "future")
   ; 3
   (list "family" "banana" "chocolate" "eleven" "potato"
         "business" "favorite" "energy" "history" "amazing")
   ; 4
   (list "irregular" "information" "preposition" "february"
         "literature" "homecoming" "technology" "community"
         "vegetable" "television")
   ; 5
   (list "California" "anniversary" "university"
         "intimidating" "individual" "elementary"
         "personality")
   ; 6
   (list "biodiversity" "revolutionary" "biotechnology")
   ; 7
   (list "telecommunication" "oversimplification")))

;; +-------------------------------------------------------------------------
;; | Helper Functions | 
;; +-------------------

;;; (random-list-element lst) -> any?
;;;   lst : list? (nonempty)
;;; Randomly select an element of `lst`
(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))

;;; (list-of-strings? val) -> boolean?
;;;   val : any?
;;; Determines if val is a list of strings.
(define list-of-strings?
  (lambda (val)
    (and (list? val)
         (andmap string? val))))

;;; (random-list-length lst n) -> element-of-list?
;;; lst : list?
;;; n : integer?, an integer less than or equal to (length lst)
;;; Return a random element from lst from 0 (inclusive) to n (exclusive)
(define random-list-length
  (lambda (lst n)
    (list-ref lst (random n))))

;;; (longer-length syllax n) -> integer?
;;; syllax : list?, name of a syllax
;;; n : integer?
;;; Returns the smaller of (length syllax) and n 
(define longer-length
  (lambda (syllax n)
    (if (> (length syllax) n)
        n
        (sub1 (length syllax)))))

;; +-------------------------------------------------------------
;; | Part 1 | Generating Haiku
;; +-------------------------------------------------------------

;;; (phrase n syllax) -> string?
;;; n : integer?, the number of syllables
;;; syllax : list?, name of the syllax to get the word from
;;; Return a string of word(s) with n syllables from the syllax
(define phrase
  (lambda (n syllax)
    (if (= 0 n)
        ""
        (let* ([remain-sylla (add1 (longer-length syllax n))]
               [random-sylla-list (random-list-length syllax remain-sylla)]
               [index (index-of syllax random-sylla-list)])
          (if (null? random-sylla-list)
              (phrase n syllax)
              (string-normalize-spaces (string-append (random-list-element random-sylla-list)
                                                      " "
                                                      (phrase (- n index) syllax))))))))

;; +-------------------------------------
;; | Tests | Procedure (phrase n syllax)
;; +-------------------------------------

#|
> (phrase 7 csc151-syllax)
"pair section abstraction lab"
1 + 2 + 3 + 1 = 7

> (phrase 7 csc151-syllax)
"data cdr string match match"
2 + 2 + 1 + 1 + 1 = 7

> (phrase 7 csc151-syllax)
"list generality match"
1 + 5 + 1 = 7

> (phrase 7 csc151-syllax)
"sort sort decomposition"
1 + 1 + 5 = 7

> (phrase 7 csc151-syllax)
"triskaidekaphobia"
7 = 7

> (phrase 7 csc151-syllax)
"sort abstraction sort compose"
1 + 3 + 1 + 2 = 7

> (phrase 20 grinnell-syllax)
"Congregationalist Congregationalist need-blind Mary B. James train black"
6 + 6 + 2 + 4 + 1 + 1 = 20

> (phrase 100 csc151-syllax)
"Scheme triskaidekaphobia test cdr eboard DrRacket one-fifty-one map binary
defenestration lab Scheme MP sort jelly abstraction one-fifty-one triskaidekaphobia
algorithm DrRacket triskaidekaphobia decomposition image triskaidekaphobia collaborate
match Racket abstraction SoLA triskaidekaphobia map"

TOO MUCH TO COUNT!!!
|#

;;; (haiku syllax) -> string?
;;; syllax : list?, name of a syllax
;;; Returns a string that has the three lines of the haiku all of them having \n afterwards
(define haiku
  (lambda (syllax)
    (string-append (phrase 5 syllax)
                   "\n"
                   (phrase 7 syllax)
                   "\n"
                   (phrase 5 syllax)
                   "\n")))

;; +-------------------------------------
;; | Tests | Procedure (haiku syllax)
;; +-------------------------------------

#|
> (haiku csc151-syllax)
"generality\nrecurse humanities fold\neboard cons lab sort\n"

> (haiku csc151-syllax)
"lab recursion string\ntriskaidekaphobia\ncadr document\n"

> (haiku csc151-syllax)
"eboard image list\ndocument cadr Racket\ncomputer section\n"

> (haiku csc151-syllax)
"collaborate string\ngenerality lambda\nMP match compose\n"

> (haiku csc151-syllax)
"one-fifty-one cons\nconfusing one-fifty-one\ncomputer eboard\n"

> (haiku csc151-syllax)
"generality\nalgorithm car lab lab\nboolean sort cons\n"
|#

;; +-----------------------------------------
;; | Samples | Haiku Samples from my-syllax
;; +-----------------------------------------

#|
> (display (haiku my-syllax))
information bus
again bus pizza love hug
february me

> (display (haiku my-syllax))
favorite pizza
eleven amazing car
elementary

> (display (haiku my-syllax))
favorite thursday
oversimplification
California

> (display (haiku my-syllax))
university
California car you
banana seven
|#

;; +-------------------------------------------------------------
;; | Part 2 | Extracting Words and Other Utilities
;; +-------------------------------------------------------------

;;; (extract-words str) -> list?
;;; str : string?, a text written in one string
;;; Return a list of words found in the text
;;; I got this from my own function written in Mini Project 3 
(define extract-words
  (lambda (str)
    (filter non-empty-string? (rex-split-string (rex-any-of (rex-repeat (rex-char-set " \n\t\r"))
                                                            (rex-concat (rex-repeat (rex-char-set ".,;:!?_\"<>|/^%$#"))
                                                                        (rex-repeat-0 (rex-string " ")))
                                                            (rex-char-set "0123456789")) str))))

;; +---------------------------------------
;; | Tests | Procedure (extract-words str)
;; +---------------------------------------

(test-equal? "standard" (extract-words "we love cats") '("we" "love" "cats"))
(test-equal? "multiple spaces" (extract-words "we      love      cats") '("we" "love" "cats"))
(test-equal? "upper case and lower case" (extract-words "HI HELLO cat dog MY") '("HI" "HELLO" "cat" "dog" "MY"))
(test-equal? "empty string" (extract-words "") '())
(test-equal? "singleton" (extract-words "hi") '("hi"))
(test-equal? "punctuations" (extract-words "hi. hello! what?") '("hi" "hello" "what"))
(test-equal? "apostrophes" (extract-words "don't can't doesn't") '("don't" "can't" "doesn't"))
(test-equal? "white spaces" (extract-words "one\ntwo\tthree\rfour five") '("one" "two" "three" "four" "five"))
(test-equal? "numbers" (extract-words "1 2 3 4 5") '())

;;; (list-contains? lst val) -> boolean?
;;; lst : list?
;;; val : string?, char?, boolean?, list?
;;; Determines whether or not lst has the val by returning #t or #f
(define list-contains?
  (lambda (lst val)
    (if (null? lst)
        #f
        (if (equal? (car lst) val)
            #t
            (list-contains? (cdr lst) val)))))

;; +--------------------------------------------
;; | Tests | Procedure (list-contains? lst val)
;; +--------------------------------------------

(test-true "one match" (list-contains? '(1 2 3) 2))
(test-true "list match" (list-contains? '(1 (1 2) 3) '(1 2)))
(test-true "many matches" (list-contains? '(1 2 1 1 3) 1))
(test-true "match at beginning" (list-contains? '(1 2 3 2 3 2 3 2 3) 1))
(test-true "match at end" (list-contains? '(3 2 3 2 3 2 1) 1))
(test-false "different type match" (list-contains? '(1 2 3) "1"))
(test-false "empty-list match" (list-contains? '() 1))
(test-false "empty string in empty list" (list-contains? '() ""))
(test-false "match" (list-contains? '() 5))
(test-false "no match" (list-contains? '(1 2 3) 4))

;;; (dedup lst) -> list?
;;; lst : list?
;;; Remove all the duplicates from the list and keeping the same order
(define dedup
  (lambda (lst)
    (if (null? lst)
        null
        (cons (car lst)
              (dedup (filter                               ;; Filters and places the items that are not 
                      (lambda (item)                       ;; the same as (car lst) into a list that is 
                        (not (equal? (car lst) item)))     ;; then used in the recursive call
                      lst))))))

;; +-------------------------------------
;; | Tests | Procedure (dedup lst)
;; +-------------------------------------

(test-equal? "standard" (dedup '(1 1 2 3)) '(1 2 3))
(test-equal? "empty list" (dedup '()) '())
(test-equal? "no duplicates" (dedup '(1 2 3 4)) '(1 2 3 4))
(test-equal? "duplicates at end" (dedup '(1 2 3 4 5 5 6 6 6 7 7 7 7)) '(1 2 3 4 5 6 7))
(test-equal? "singleton" (dedup '(1)) '(1))
(test-equal? "list in list" (dedup '(1 2 2 (2 2 3 3 4 4) 3 4)) '(1 2 (2 2 3 3 4 4) 3 4))
(test-equal? "duplicates at beginning and end" (dedup '(1 1 3 4 5 7 8 9 9 10 10 10 10)) '(1 3 4 5 7 8 9 10))
(test-equal? "duplicates at random location" (dedup '(1 2 1 2 2 2 1 2 3 2 1 3 1)) '(1 2 3))
(test-equal? "duplicates with different type" (dedup '(1 "1" #\1 (1))) '(1 "1" #\1 (1)))

;; +-------------------------------------------------------------
;; | Part 3 | Slightly Angry Libs 
;; +-------------------------------------------------------------

;;; (replace-characters original-characters new-characters words) -> list?
;;; original-characters : list?, a list of strings
;;; new-characters : list?, a list of strings
;;; words : list?, a list of strings
;;; Return 'words' but where occurences of strings in 'original-characters' are
;;; replaced by random strings in 'new-characters'
(define replace-characters 
  (lambda (original-characters new-characters words)
    (if (null? words)
        '()
        (let* ([first-string (car words)]
               [tail-strings (cdr words)]
               [recursive-call (replace-characters original-characters new-characters tail-strings)])
          (if (list-contains? original-characters
                              first-string)
              (append (list (random-list-element new-characters))
                      recursive-call)
              (append (list first-string)
                      recursive-call))))))

;;; (replace-words catergory words) -> list?
;;; category : list?, a list of strings
;;; words : list?, a list of strings
;;; Return 'words' but where occurences of strings in 'category'
;;; are replaced by random strings in 'category'
(define replace-words
  (lambda (category words)
    (if (null? words)
        '()
        (let* ([first-string (car words)]
               [tail-strings (cdr words)]
               [recursive-call (replace-words category tail-strings)])
          (if (list-contains? category
                              first-string)
              (append (list (random-list-element category))
                      recursive-call)
              (append (list first-string)
                      recursive-call))))))

;;; (maddish-libs) -> string?
;;; Returns a sample piece of text with its noun, plural noun
;;; verb-ing, adjective, and abverb randomly chosen each time.
;;; The names of the character are also switched randomly. 
(define maddish-libs
  (let ([text (extract-words (file->string "sample.txt"))]
        [original-characters '("John" "Kate" "Matt")]
        [new-characters '("Jake" "Julie" "Karah" "Joseph" "Blake" "Alice")]
        [noun '("cat" "dog" "monkey" "fish" "human" "mountain" "forest" "canyon")]
        [plural-noun '("faces" "computers" "ears" "fingers"
                               "leaves" "trees" "colleges" "beds" "tables")]
        [verb-ing '("fishing" "climbing" "defenestrating" "drinking" "eating" "crying" "falling" "punching")]
        [adjective '("weird" "unusual" "new" "exciting" "sad" "melancholy" "amazing" "fabulous" "lovely")]
        [adverb '("thankfully" "gladly" "terribly" "unnaturally" "uselessly" "very" "verbally" "wrongly")])
    (lambda ()
      (let* ([new-text-1 (replace-characters original-characters
                                             new-characters
                                             text)]
             [new-text-2 (replace-words noun new-text-1)]
             [new-text-3 (replace-words plural-noun new-text-2)]
             [new-text-4 (replace-words verb-ing new-text-3)]
             [new-text-5 (replace-words adjective new-text-4)]
             [new-text-6 (replace-words adverb new-text-5)])
        (reduce (lambda (s1 s2) (string-append s1 " " s2))
                new-text-6)))))

;; +-------------------------------------
;; | Two Samples of 'maddishb-libs' |
;; +-------------------------------------

#|
"A vacation is when Alice take a trip to some melancholy place with
their lovely family Usually Joseph go to some place that is near a
human or up on a forest  A good vacation place is one where Karah can
terribly ride leaves or play tik-tak-toe or go hunting for faces Jake
like to spend their time fishing or punching  When parents go on a
vacation Blake gladly spend their time drinking three leaves a day and
fathers play golf and mothers sit around punching Last summer Jake fell
in a fish and got poison ivy all over their face Their family is going
to go to six flags and Joseph will wrongly practice defenestrating
Parents need vacations more than kids because parents are always unnaturally
new and because they have to very work ten hours every day all year making
enough ears to pay for the vacation"
|#

#|
"A vacation is when Alice take a trip to some melancholy place with their
fabulous family Usually Joseph go to some place that is near a fish or up on a dog
A good vacation place is one where Alice can very ride computers or play
tik-tak-toe or go hunting for ears Blake like to spend their time punching or falling
When parents go on a vacation Julie very spend their time punching three tables a day
and fathers play golf and mothers sit around defenestrating Last summer Julie fell in a
cat and got poison ivy all over their face Their family is going to go to six flags and
Alice will uselessly practice climbing Parents need vacations more than kids because parents
are always wrongly melancholy and because they have to terribly work ten hours every day all
year making enough colleges to pay for the vacation"
|#

;; +-------------------------------------------------------------
;; | Part 4 | Identifying Syllables
;; +-------------------------------------------------------------

;; Matches vowels and 'y'
(define vowels
  (rex-char-set "aeiouy"))

;; Matches consonants 
(define consonants
  (rex-char-set "bcdfghjklmnpqrstvwxz"))

;; Matches ending with e
(define end-with-e
  (rex-concat (rex-char-antiset "l")
              (rex-string "e")
              (rex-end-of-string)))

;; Matches with "oe" and "ie" vowel sequences
(define vowel-sequence
  (rex-any-of (rex-string "eo")
              (rex-string "ie")))

;; Matches with consonants - "oo" - "n"
(define c-oo-c
  (rex-concat consonants
              (rex-string "oo")
              (rex-string "n")))

;; Matches with vowel pairs (vowel sequences that is only one syllable)
;; Also not matches with "our")
(define vowel-pair
  (rex-any-of (rex-string "ai")
              (rex-string "ea")
              (rex-string "io")
              (rex-string "ei")
              (rex-string "oa")
              (rex-string "you")
              (rex-concat (rex-string "ou")
                          (rex-char-antiset "r"))))

;; Matches with vowel - "y"
(define vowel-y
  (rex-concat (rex-repeat vowels)
              (rex-string "y")))

;; Matches with "qu" - vowel
(define qu-vowel
  (rex-concat (rex-string "qu")
              vowels))

;;; (count-matches rex word) -> integer?
;;; rex : rex-pattern?
;;; word : string?
;;; Return the number of times 'word' matches rex
(define count-matches
  (lambda (rex word)
    (length (rex-find-matches rex word))))

;;; (syllables word) -> integer?
;;; word : "string"
;;; Returns the number of syllables in 'word'
(define syllables
  (lambda (word)
    (let ([vowels (count-matches vowels word)]
          [end-with-e (count-matches end-with-e word)]
          [vowel-sequence (count-matches vowel-sequence word)]
          [vowel-pair (count-matches vowel-pair word)]
          [vowel-y (count-matches vowel-y word)]
          [qu-vowel (count-matches qu-vowel word)]
          [c-oo-c (count-matches c-oo-c word)])
      (reduce-left - (list vowels vowel-pair c-oo-c end-with-e vowel-y qu-vowel)))))

;; Success
#|
(syllables "science")
2
(syllables "defenestrate")
4
(syllables "air")
1
(syllables "balloon")
2
(syllables "hour")
2
(syllables "heir")
1
(syllables "bouyant")
2
(syllables "conquer")
2
(syllables "coalesce")
2
(syllables "blondie")
2
(syllables "adelaide")
3
|#

;; Fail
#|
(syllables "courses")
3
(syllables "bureaux")
3
|#

;;; (num-syllables? num word) -> boolean?
;;; num : integer?
;;; word : string?
;;; Return #t if 'word' has 'num' syllables, otherwise #f
(define num-syllables?
  (lambda (num word)
    (= (syllables word) num)))

;;; (filter-num-syllables num lst) -> list?
;;; num : integer?
;;; lst: list?
;;; Returns a list of elements that have 'num' syllables from 'lst'
(define filter-num-syllables
  (lambda (num lst)
    (filter (section num-syllables? num <>) lst)))

;;; Returns a syllax of words of 1-7 syllables that is in the first thousand line of the text "Jane Eyre"
(define jane-eyre-syllax
  (let* ([jane-eyre-text (dedup (extract-words (string-join (take (file->lines "1260-0.txt") ;; Returns each words that appear 
                                                                  1000))))]                  ;; in the first 1000 lines of "Jane Eyre" ONCE 
         [syllable-list (lambda (num)                                   ;; num : integer?
                          (filter-num-syllables num jane-eyre-text))])  ;; Returns a list of words with 'num' syllables from 'jane-eyre-text'
    (list
     (list)
     (syllable-list 1)
     (syllable-list 2)
     (syllable-list 3)
     (syllable-list 4)
     (syllable-list 5)
     (syllable-list 6)
     (syllable-list 7))))

;; +-------------------------------------------------
;; | Haiku Samples from 'jane-eyre-syllax |
;; +-------------------------------------------------

#|
> (display (haiku jane-eyre-syllax))
snatch analysis
pigeons waistcoat Can’t tend
noxious duty
|#

#|
> (display (haiku jane-eyre-syllax))
regaining whirls blast
glad hard-wrung Henry moonlight
wind justly Turk grasp
|#

#|
> (display (haiku jane-eyre-syllax))
spacious hung right
themselves arms quivered
unutterable
|#

;; +-------------------------------------------------------------
;; | Part 5 | Rhyming
;; +-------------------------------------------------------------

;;; (might-rhyme? word1 word2) -> boolean?
;;; word1 : string?
;;; word2 : string?
;;; Return #t if word1 and word2 rhymes, otherwise returns #f
(define might-rhyme?
  (lambda (word1 word2)
    (if (or (> 3 (string-length word1))
            (> 3 (string-length word2)))
        #f
        (string-suffix? word1
                        (substring word2
                                   (- (string-length word2) 3)
                                   (string-length word2))))))

;; Examples that do rhyme but do not pass test
#|
> (might-rhyme? "rhyme" "time")
#f
> (might-rhyme? "rhyme" "climb")
#f
> (might-rhyme? "find" "inclinded")
#f
> (might-rhyme? "cat" "rat")
#f
> (might-rhyme? "pair" "where")
#f
> (might-rhyme? "pair" "aware")
#f
> (might-rhyme? "you" "blue")
#f
> (might-rhyme? "you" "crew")
#f
> (might-rhyme? "awe" "draw")
#f
> (might-rhyme? "blur" "were")
#f
|#

;; Examples that do not rhyme but pass test
#|
> (might-rhyme? "all" "shall")
#t
> (might-rhyme? "paid" "said")
#t
> (might-rhyme? "read" "lead") ;; Depending on the tenses
#t
> (might-rhyme? "suction" "zion")
#t
> (might-rhyme? "your" "hour")
#t
> (might-rhyme? "learn" "barn")
#t
> (might-rhyme? "though" "cough")
#t
> (might-rhyme? "laos" "chaos")
#t
> (might-rhyme? "shall" "ball")
#t
> (might-rhyme? "impulse" "false")
#t
|#

#|
Include rhymes:
- For words with only three syllables, check if their last two syllables match
- Allow words that end with -air to rhyme with -are and -ere
- Allow for -aw to rhyme with -awe
- Allow for -ind to rhyme with -inded
- Allow -ur to rhyme with -er, -ere, -urr, -err
- "you" would rhyme with words that have the -u sound at the end
- Allow -ime to rhyme with -yme


Exclude rhymes:
- For -arn determines whether or not there is a consonant that precedes it
- For "shall" it would not rhyme with regular words with ending -all
- For -lse determines what is the vowel that precedes it
- For -ugh determines what is the vowel that precedes it and also consonant precedes that vowel
- For -aid determines what consonant precedes it
- For -our determines what consonant precedes it (y different than h, fl, s)
- For -ion usually -tion would rhyme but not with other consonant in front of -ion
- "chaos" rhymes with words that end in -oss rather than -aos
|#

;; Matches every letter in alphabet 
(define alphabet
  (rex-char-range #\a #\z))

;; Matches with every vowels 
(define vowel
  (rex-char-set "aeiou"))

;;; (end-with ptn) -> rex?
;;; ptn : rex-pattern?
;;; Returns a new pattern that find endings of 'ptn'
(define end-with
  (lambda (ptn)
    (rex-concat (rex-repeat-0 alphabet)
                (rex-string ptn))))

;;; (end-with-comp rex) -> rex?
;;; ptn1 : rex-pattern?
;;; ptn2 : rex-pattern?
;;; Returns a new pattern that find endings of 'ptn1' or 'ptn2'
(define end-with-comp
  (lambda (ptn1 ptn2)
    (rex-any-of (end-with ptn1)
                (end-with ptn2))))

;; Matches with "aw" and "awe" endings
(define aw-awe
  (end-with-comp "aw" "awe"))

;; Matches with "ind" "inded" endings
(define ind-ined
  (end-with-comp "ind" "ined"))

;; Matches with "ime" "yme" endings
(define ime-yme
  (end-with-comp "ime" "yme"))

;; Matches with "ou" and some "u" sounding combinations
(define ou-u-sound
  (rex-any-of (end-with-comp "ou" "oo")
              (end-with-comp "rew" "ue")
              (end-with-comp "do" "ru")))


;;; almost-same?
;;; word1 : string?
;;; word2 : string?
;;; There are two conditions in order to get true:
;;; The two strings must be equal length
;;; Every letter except the first letter of the two string must match
;;; Otherwise, return #f
(define almost-same?
  (lambda (word1 word2)
    (let ([length1 (string-length word1)]
          [length2 (string-length word2)]
          [substring1 (substring word1 1)]
          [substring2 (substring word2 1)])
      (and (= length1 length2)
           (equal? substring1 substring2))
      #f)))

;;; (rhymes? word1 word2) -> boolean?
;;; word1 : string?
;;; word2 : string?
;;; Return #t if 'word1' and 'word2' rhymes, otherwise return #f
(define rhymes?
  (lambda (word1 word2)
    (let ([rex-match (lambda (rex) (and (rex-matches? rex word1)
                                        (rex-matches? rex word2)))])
      (cond
        [(equal? word1 word2)]
        [(rex-match aw-awe)]
        [(rex-match ind-ined)]
        [(rex-match ime-yme)]
        [(rex-match ou-u-sound)]
        [(almost-same? word1 word2)]
        [(might-rhyme? word1 word2)]
        [else
         #f]))))

;; +-------------------------------------------------
;; | Examples of (rhymes? word1 word2) |
;; +-------------------------------------------------

#|
> (rhymes? "hello" "hello")
#t
> (rhymes? "draw" "awe")
#t
> (rhymes? "find" "inclined")
#t
> (rhymes? "time" "thyme")
#t
> (rhymes? "you" "crew")
#t
> (rhymes? "you" "sue")
#t
> (rhymes? "robbery" "sobbery")
#t
> (rhymes? "mind" "find")
#t
|#

;;; (rhymes-with word words) -> list?
;;; word : string?
;;; words : list?
;;; Return a list of words that rhyme with 'word' from 'words' 
(define rhymes-with
  (lambda (word words)
    (filter (section rhymes? word <>) words)))

;;; (random-word-from-file filename) -> string?
;;; filename : string?, a filename that has list of words
;;; Return a random word from the file
(define random-word-from-file
  (lambda (filename)
    (random-list-element (extract-words (file->string filename)))))

;;; Produces a random three word string
(define three-word-line
  (lambda ()
    (string-append (random-word-from-file "names.txt")
                   " "
                   (random-word-from-file "adverbs.txt")
                   " "
                   (random-word-from-file "verbs.txt")
                   " ")))

;;; (full-line word) -> string?
;;; word : string?
;;; Return a line of the quatrain 
(define full-line
  (lambda (word)
    (string-append (three-word-line)
                   word)))

;; List of words from "nouns.txt"
(define word-list
  (extract-words (file->string "nouns.txt")))
  
;;; (abab words) -> string?
;;; words : list?
;;; Return a random quatrain 
(define abab
  (lambda (words)
    (let* ([last-word1 (random-list-element words)]
           [last-word2 (random-list-element words)]
           [rhyme-last-word1 (random-list-element (rhymes-with last-word1 words))]
           [rhyme-last-word2 (random-list-element (rhymes-with last-word2 words))])
      (string-append (full-line last-word1)
                     "\n"
                     (full-line rhyme-last-word1)
                     "\n"
                     (full-line last-word2)
                     "\n"
                     (full-line rhyme-last-word2)))))                

;; +-------------------------------------------------
;; | Quatrain Samples from (abab words) |
;; +-------------------------------------------------

#|
> (display (abab word-list))
Javone cleanly switch frame
Joannah messily yell nickname
Rohit potentially scorch aquifer
Nicosha urgently invent loafer
|#

#|
> (display (abab word-list))
Camaron affectionately analyze contact
Javar unhappily steal tract
Jessey wisely strip subexpression
Dorota brightly pour fiction
|#

#|
> (display (abab word-list))
Alexander highly excuse guilt
Eryka triumphantly beat kilt
Meira enticingly move translation
Navin accusingly sew inflammation
|#





          















  
        
         
                

  


