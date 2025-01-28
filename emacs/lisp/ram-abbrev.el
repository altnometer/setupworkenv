;;* global-abbrev-table

;; credit to http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;; (clear-abbrev-table global-abbrev-table)

(when (boundp 'global-table)
  (clear-abbrev-table global-table))

(define-abbrev-table 'global-abbrev-table
  '(
    ;; net addrev
    ("1t" "first")
    ("2d" "second")
    ("3d" "third")
    ("ty" "thank you")
    ("□" "□")

    ("Therefor" "Therefore")
    ("therefor" "therefore")

    ;; Pāli
    ("AA" "Ā")
    ("aa" "ā")
    ("II" "Ī")
    ("ii" "ī")
    ("UU" "Ū")
    ("uu" "ū")
    ;; the rest do not work as the symbols used are not word parts
    ;; ("\"N" "Ṅ")
    ;; ("\"n" "ṅ")
    ;; (".M" "Ṃ")
    ;; (".m" "ṃ")
    ;; ("~N" "Ñ")
    ;; ("~n" "ñ")
    ;; (".T" "Ṭ")
    ;; (".t" "ṭ")
    ;; (".D" "Ḍ")
    ;; (".d" "ḍ")
    ;; (".N" "Ṇ")
    ;; (".n" "ṇ")
    ;; (".L" "Ḷ")
    ;; (".l" "ḷ")
    ))

;;* clojure-mode-abbrev-table

(setq clojure-abbrevs
        '(
          ("dc" "declare")
          ("df" "defn")
          ("dm" "defmacro")
          ("dt" "deftest")
          ("me" "macroexpand-1")
          ("pl" "println")
          ("pr" "print")))

(with-eval-after-load "clojure"
  (when (boundp 'clojure-mode-abbrev-table)
    (clear-abbrev-table clojure-mode-abbrev-table))

  (when (boundp 'cider-repl-mode-abbrev-table)
    (clear-abbrev-table cider-repl-mode-abbrev-table))

  (define-abbrev-table 'clojure-mode-abbrev-table
    clojure-abbrevs)

  (define-abbrev-table 'cider-repl-mode-abbrev-table
    clojure-abbrevs))

;;* elisp emacs-lisp

(with-eval-after-load "abbrev"

  (when (boundp 'lisp-mode-abbrev-table)
    (clear-abbrev-table lisp-mode-abbrev-table))

  (setq emacs-lisp-abbrevs
        '(
          ("df" "defun")
          ("la" "lambda")
          ("lam" "lambda")
          ;;
          ))

  (define-abbrev-table 'lisp-mode-abbrev-table
    emacs-lisp-abbrevs))


;;* racket-repl-mode-abbrev-table

(setq racket-abbrevs
      '(
        ;; ("be" "begin")
        ("usquare" "⃞")
        ("ca" "call-with-current-continuation")
        ("df" "define")
        ("di" "display")
        ("la" "lambda")
        ("lam" "lambda")
        ;; ("pr" "procedure?")
        ))

(with-eval-after-load 'racket-mode
  (when (boundp 'racket-repl-mode-abbrev-table)
    (clear-abbrev-table racket-repl-mode-abbrev-table))

  (when (boundp 'racket-mode-abbrev-table)
    (clear-abbrev-table racket-mode-abbrev-table))

  (define-abbrev-table 'racket-mode-abbrev-table
    racket-abbrevs)

  (define-abbrev-table 'racket-repl-mode-abbrev-table
    racket-abbrevs))

;;* org-mode

(with-eval-after-load "org"
  (when (boundp 'org-mode-abbrev-table)
    (clear-abbrev-table org-mode-abbrev-table))

  (setq org-abbrevs
        '(

          ;;** Greek

          ("theta" "Θ")

          ;;** Latin

          ("eg" "e.g.,")
          ("ie" "i.e.,")

          ("~c" "~cons~")

          ;;** a

          ("ai" "artificial intelligence")

          ("aor" "area of responsibility")
          ("ab" "abstraction")

          ("ag" "argument")
          ("ags" "arguments")
          ("arg" "argument")
          ("args" "arguments")

          ("arit" "arithmetic")
          ("arith" "arithmetic")

          ("alg" "algorithm")
          ("algs" "algorithms")
          ;; ("app" "application")
          ("appn" "application")
          ("apps" "applications")
          ("appd" "applied")

          ("appr" "approach")

          ("arch" "architecture")

          ("assd" "associated")

          ("assm" "assumption")
          ("assms" "assumptions")
          ("assmg" "assuming")

          ("auty" "automatically")
          ("autom" "automatically")
          ("autly" "automatically")

          ;;** b

          ("bl" "buffer-local")

          ("bc" "because")
          ("bf" "before")

          ("bh" "behavior")
          ("bhs" "behaviors")
          ("bhe" "behave")
          ("bhes" "behaves")

          ("bn" "between")

          ("bt" "binary tree")
          ("bts" "binary trees")

          ;;** c

          ("calc" "calculate")
          ("calcs" "calculates")
          ("calcn" "calculation")
          ("calcg" "calculating")

          ("catg" "category")
          ("catgs" "categories")
          ("catzd" "categorized")
          ("catzn" "categorization")

          ("cd" "could")

          ("chr" "character")
          ("chrs" "characters")
          ("chars" "characters")
          ("charz" "characterize")
          ("charzd" "characterized")
          ("charc" "characteristic")
          ("Cider" "CIDER")
          ("cj" "clojure")
          ("Cj" "Clojure")

          ("clf" "clausal form of logic")

          ("clp" "classpath")
          ("clsp" "clojure-lsp")

          ("cmd" "command")
          ("cmds" "commands")

         
          ("coll" "collection")
          ("colls" "collections")

          ("com" "communication")
          ("comm" "communication")

          ("comn" "computation")
          ("coml" "computational")
          ("comg" "computing")

          ("comr" "computer")
          ("comrs" "computers")
          ("comp" "compute")
          ("compn" "computation")
          ("compns" "computations")
          ("compl" "computational")
          ("compg" "computing")

          ("compl" "completion")
          ("complg" "completing")

          ("comds" "commands")

          ("concl" "conclusion")

          ("concr" "concurrent")
          ("concrt" "concurrent")
          ("concrc" "concurrency")
          ("concry" "concurrently")

          ("cond" "condition")
          ("conds" "conditions")

          ("cont" "contain")
          ("contg" "containing")
          ("conts" "contains")

          ("conu" "continue")

          ("conf" "configure")
          ("confd" "configured")
          ("confg" "configuring")
          ("confn" "configuration")
          ("confns" "configurations")

          ("cor" "correct")
          ("corr" "correct")
          ("corry" "correctly")
          ("cory" "correctly")

          ;;** d

          ("dec" "declare")
          ("decn" "declaration")

          ("def" "define")
          ("defd" "defined")
          ("defg" "defining")
          ("defn" "definition")
          ("defns" "definitions")


          ("desc" "describe")
          ("descn" "description")

          ("detn" "determine")

          ("dev" "develop")
          ("devt" "development")

          ("df" "different")
          ("dfc" "difference")
          ("dfte" "differentiate")
          ("dfl" "differential")
          ("dfy" "differently")

          ("dif" "different")
          ("difc" "difference")
          ("difte" "differentiate")
          ("difl" "differential")
          ("dify" "differently")

          ("diff" "difficult")
          ("diffy" "difficulty")
          ("diffs" "difficulties")

          ("dp" "dependency")
          ("dps" "dependencies")

          ("dft" "default")
          ("dr" "derivative")
          ("drs" "derivatives")


          ;;** e

          ("el" "element")
          ("els" "elements")
          ("env" "environment")
          ("envs" "environments")

          ("ev" "evaluate")
          ("eva" "evaluate")
          ("evd" "evaluated")
          ("evg" "evaluating")
          ("evn" "evaluation")

          ("evt" "everything")
          ("evrg" "everything")

          ("excn" "exception")
          ("excns" "exceptions")

          ("expd" "expressed")
          ("expn" "expression")
          ("expns" "expressions")

          ("exper" "experience")
          ("experd" "experience")
          ("expr" "experience")
          ("exprd" "experienced")
          ("expers" "experiences")
          ("exprs" "experiences")

          ("exl" "example")
          ("exls" "examples")
          ("ext" "execute")
          ("exts" "executes")
          ("exn" "execution")

          ;;** f

          ("fe" "for example")

          ("fn" "function")
          ("fns" "functions")
          ("fnl" "functional")
          ("fnly" "functionally")
          ("fny" "functionality")
          ("fnty" "functionality")

          ("fsa" "finite state automata")
          ("FSA" "FSA")
          ("fsg" "finite state generator")
          ("fsr" "finite state recognizer")

          ;;** h


          ("hier" "hierarchy")
          ("hiers" "hierarchies")
          ("hl" "headline")
          ("hls" "headlines")

          ;;** i

          ("ifif" "if and only if")

          ("inst" "instance")
          ("insts" "instances")
          ("instt" "instantiate")

          ("imp" "implement")
          ("imps" "implements")
          ("impd" "implemented")
          ("impg" "implementing")
          ("impn" "implementation")
          ("impns" "implementations")

          ("impl" "implication")
          ("impls" "implications")

          ("impt" "important")

          ("inct" "inconsistent")
          ("incty" "inconsistency")
          ("inctys" "inconsistencies")
          ("inctl" "inconsistently")

          ("indv" "individual")
          ("indvs" "individuals")

          ("inf" "information")

          ("init" "initialize")
          ("initd" "initialized")
          ("initn" "initiation")
          ("initg" "initializing")
          ("initzn" "initialization")

          ("intg" "interesting")

          ("intr" "interpreter")
          ("inter" "interpreter")
          ("interd" "interpreted")
          ("intrg" "interpreting")
          ("interg" "interpreting")
          ("intern" "interpretation")
          ("interns" "interpretations")

          ("inv" "invoke")
          ("invd" "invoked")
          ("invg" "invoking")
          ("invn" "invocation")

          ("jvm" "JVM")

          ("kw" "keyword")
          ("kws" "keywords")

          ;;** l

          ("labelled" "labeled")
          ("latex" "LaTeX")
          ("lib" "library")
          ("libs" "libraries")
          ("lbs" "libs")
          ("Lsp" "LSP")

          ("lc" "logical consequence")
          ("lg" "language")
          ("lgs" "languages")

          ;;** m

          ("mach" "machine")
          ("machs" "machines")

          ("math" "mathematics")
          ("maths" "mathematics")
          ("mathl" "mathematical")

          ("mech" "mechanism")
          ("mechm" "mechanism")
          ("mechl" "mechanical")

          ("mem" "member")
          ("mems" "members")

          ("multm" "multimethod")
          ("multms" "multimethods")

          ("mt" "manipulate")
          ("mtg" "manipulating")
          ("mec" "mechanism")

          ;;** n

          ("nams" "namespace")
          ("namss" "namespaces")
          ("nb" "number")
          ("nbs" "numbers")

          ("nl" "natural language")

          ("nm" "number")
          ("nms" "numbers")

          ("nond" "nondeterminism")
          ("nondc" "nondeterministic")
          ("nondy" "nondeterministically")

          ;;** o

          ("ob" "object")
          ("obs" "objects")
          ("occ" "occurrence")
          ("occs" "occurrences")

          ("oa" "org-agenda")

          ;;** p

          ("par" "paragraph")
          ("pars" "paragraphs")

          ("parl" "parallel")
          ("parld" "parallelized")
          ("parlg" "parallelizing")
          ("parlm" "parallelism")
          ("parln" "parallelization")
          ("parlz" "parallelize")


          ("parm" "parameter")
          ("parmd" "parameterized")
          ("parmg" "parameterizing")
          ("parmn" "parameterization")
          ("parms" "parameters")
          ("parmz" "parameterize")
          ("parn" "parentheses")


          ("pg" "program")
          ("pgs" "programs")
          ("pgg" "programming")
          ("pgly" "programmatically")
          ("pgr" "programmer")
          ("pgrs" "programmers")

          ;;*** pr
          ("pr" "procedure")
          ("prs" "procedures")
          ("prl" "procedural")
          ("pry" "procedurally")

          ("prb" "problem")
          ("prbs" "problems")

          ("prd" "predicate")
          ("pred" "predicate")
          ("prds" "predicates")
          ("preds" "predicates")

          ("prec" "preceding")
          ("prev" "previous")

          ("prj" "project")
          ("prjs" "projects")

          ("pri" "principle")

          ("proc" "process")
          ("procg" "processing")

          ("prop" "property")
          ("props" "properties")

          ("prot" "prototype")
          ("prots" "prototypes")

          ("prt" "protocol")
          ("prts" "protocols")

          ("qr" "query")
          ("qrs" "queries")

          ;;** r

          ("rc" "recursive")
          ("rcl" "recursively")
          ("rcn" "recursion")
          ("recv" "receiver")

          ("ref" "reference")
          ("refs" "references")
          ("refg" "referencing")

          ("rep" "represent")
          ("reps" "represents")
          ("repd" "represented")
          ("repn" "representation")
          ("repg" "representing")

          ("Repl" "REPL")
          ("nRepl" "nREPL")

          ("req" "require")
          ("reqs" "requires")

          ("resp" "responsibility")
          ("resps" "responsibilities")

          ("rel" "relation")
          ("rell" "relational")
          ("rels" "relations")
          ("relp" "relationship")
          ("relps" "relationships")

          ("rn" "return")
          ("rnd" "returned")
          ("rng" "returning")
          ("rns" "returns")

          ;;** s

          ("seq" "sequence")
          ("seql" "sequential")

          ("sd" "should")
          ("sdt" "shouldn't")

          ("sl" "standard form of logic")

          ("sol" "solution")
          ("soln" "solution")
          ("sols" "solutions")
          ("solns" "solutions")

          ("specc" "specific")
          ("specd" "specified")
          ("specl" "special")
          ("specs" "specifies")
          ("specy" "specify")
          ("specly" "specifically")
          ("specg" "specifying")
          ("specn" "specification")
          ("specns" "specifications")

          ("sym" "symbol")
          ("symc" "symbolic")
          ("syms" "symbols")

          ("st" "structure")
          ("std" "structured")
          ("sts" "structures")

          ("stn" "standard")

          ("stt" "statement")
          ("stts" "statements")

          ("subs" "substitution")
          ("subss" "substitutions")
          ("subsg" "substituting")

          ("sucs" "success")
          ("sucsl" "successful")
          
          ("syn" "syntax")
          ("sync" "syntactical")
          ("synq" "syntax-quote")

          ("sys" "system")
          ("syss" "systems")

          ;;** t

          ("tech" "technology")
          
          ("theo" "theory")
          ("theoc" "theoretic")

          ("tr" "transaction")
          ("trs" "transactions")

          ("tq" "technique")
          ("tqs" "techniques")

          ("todo" "TODO")

          ;;** u

          ("udp" "UDP")                 ; universal design pattern
          ("und" "understand")
          ("undd" "understood")
          ("undg" "understanding")

          ;;** v

          ("val" "value")
          ("vals" "values")
          ("vl" "value")
          ("vls" "values")

          ;; ("var" "variable")
          ;; ("vars" "variables")

          ("visn" "visualization")
          ("visg" "visualizing")

          ("wo" "without")
          ("wd" "would")

          ;;** x
          ("Xt" "XTDB")

          ;;** the end
          ))

  ;; gives error "circular list ..."
  ;; (define-abbrev-table 'org-mode-abbrev-table
  ;;   (nconc racket-abbrevs org-abbrevs))
  (define-abbrev-table 'org-mode-abbrev-table
    org-abbrevs)

  (abbrev-table-put org-mode-abbrev-table
                    :parents (list auto-correct-typo-abbrev-table)))

;;* text-mode-abbrev-table

(abbrev-table-put text-mode-abbrev-table
                  :parents (list auto-correct-typo-abbrev-table))
