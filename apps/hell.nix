# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Shell-like Haskell REPL (GHCi).
# “Hell” is like “Shell” but without “S” and with “H” as a first letter that means “Haskell”.

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib

, haskellPackages ? pkgs.haskellPackages

, writeText ? pkgs.writeText
, writeShellApplication ? pkgs.writeShellApplication
}:

let
  esc = lib.escapeShellArg;

  ghc = haskellPackages.ghcWithPackages (p: [
    p.turtle

    p.text
    p.bytestring

    p.directory
    p.filepath
    p.unix
    p.typed-process
    p.process

    p.containers
    p.mtl

    p.time
    p.aeson
    p.aeson-pretty
  ]);

  ghciScript = ''
    :set -XGHC2024
    :set -XUnicodeSyntax
    :set -XNoMonomorphismRestriction
    :set -XOverloadedStrings
    :set -XOverloadedRecordDot
    :set -XDuplicateRecordFields
    :set -XViewPatterns
    :set -XPackageImports
    :set -XRecordWildCards
    :set -XQuasiQuotes
    :set -XTemplateHaskell
    :set -XDerivingVia
    :set -XGeneralizedNewtypeDeriving
    :set -XDeriveAnyClass
    :set -XFunctionalDependencies
    :set -XTypeFamilies
    :set -XNoStarIsType
    :set -XUndecidableInstances

    :set prompt "λ "

    import Prelude hiding (id, (.), FilePath)

    import Data.Function hiding (id, (.))
    import Data.Functor

    import Numeric.Natural
    import Data.Ratio

    import Data.Proxy
    import Data.Kind
    import GHC.TypeLits
    import GHC.Generics

    import Data.Char
    import Data.String
    import qualified Data.Text as T
    import qualified Data.Text.IO as T
    import qualified Data.Text.Encoding as T
    import qualified Data.Text.Lazy as TL
    import qualified Data.Text.Lazy.IO as TL
    import qualified Data.Text.Lazy.Encoding as TL
    import qualified Data.Text.Lazy.Builder as TLB
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Builder as BSB
    import qualified Data.ByteString.Char8 as BSC
    import qualified Data.ByteString.Lazy as BSL
    import qualified Data.ByteString.Lazy.Char8 as BSLC
    import qualified Data.ByteString.Short as BSS
    import qualified Text.Printf as Printf

    import qualified Data.List.NonEmpty as NE
    import qualified Data.List as L

    import qualified Data.Aeson as J
    import qualified Data.Aeson.Encode.Pretty as J

    import qualified Data.Time as Time

    import Control.Arrow
    import Control.Category
    import Control.Monad

    import System.Environment
    import System.Directory

    import Turtle hiding ((<&>), (%))
    import qualified Turtle.Bytes as Bytes

    -- These unicode symbols can be typed using Finnish keyboard layout.
    :{
    -- Finnish layout: AltGr + ö
    ø ∷ Monoid m ⇒ m
    ø = mempty

    -- Finnish layout: AltGr + 5
    (‰) ∷ Semigroup a ⇒ a → a → a
    (‰) = (<>)

    -- Finnish layout: AltGr + Shift + x
    (·) ∷ Functor f ⇒ (a → b) → f a → f b
    (·) = (<$>)

    -- Finnish layout: AltGr + x
    (×) ∷ Functor f ⇒ f a → (a → b) → f b
    (×) = (<&>)

    -- Finnish layout: AltGr + Shift + 0
    (°) ∷ Applicative f ⇒ f (a → b) → f a → f b
    (°) = (<*>)

    -- Finnish layout: AltGr + Shift + +
    (¿) ∷ Alternative f ⇒ f a → f a → f a
    (¿) = (<|>)

    unconsNE ∷ NE.NonEmpty a → (a, [a])
    unconsNE (x NE.:| xs) = (x, xs)

    -- æ — Finnish layout: AltGr + ä
    -- ß — Finnish layout: AltGr + s
    -- µ — Finnish layout: AltGr + m

    -- “ø” prefix means (m)empty input (no input)
    --     For example:
    --     - inproc  ∷ Text → [Text] → Shell Line → Shell Line
    --     - øinproc ∷ Text → [Text]              → Shell Line

    -- “ß” prefix means command is coming from a single string,
    --     but it’s not a shell string, it’s just a command string
    --     when arguments are separated by spaces.
    --     "cmd arg1 arg2" results into "cmd" + ["arg1", "arg2"]
    --     Mnemonics: “ß” for “(S)string”
    --                (both command and arguments are comming from a string/“Text”)
    --     For example:
    --     - inproc  ∷ Text → [Text] → Shell Line → Shell Line
    --     - ßinproc ∷     Text      → Shell Line → Shell Line
    --     In some other cases (e.g. aliases) “ß” means “string” (“ß” is German “S”)
    --     which means it’s a variant that uses “Text” instead of “Line”.

    -- “æ” prefix means command is coming from
    --     ([cmd, arg1, arg2] ∷ [Text]) argument instead of
    --     a (cmd ∷ Text) + ([arg1, arg2] ∷ [Text]) pair.
    --     Unsafe/partial but can be more convenient for
    --     manual prompt calls.
    --     Mnemonics: “æ” for “(A)rray”
    --                (both command and arguments are comming from an array/list)
    --     For example:
    --     - inproc  ∷ Text → [Text] → Shell Line → Shell Line
    --     - æinproc ∷    [Text]     → Shell Line → Shell Line

    -- “µ” prefix means it’s Turtle variant for working with bytes
    --     instead of Text.
    --     For example:
    --     - ßinproc  ∷ Text → Shell Line       → Shell Line
    --     - µßinproc ∷ Text → Shell ByteString → Shell ByteString

    æprocGeneric (cmd ∷ [Text]) f applyF
      = NE.nonEmpty cmd
      & maybe (fail "Empty command") (unconsNE × uncurry f × applyF)

    ßprocGeneric (cmd ∷ Text) f applyF = æprocGeneric (T.words cmd) f applyF

    -- Text variants

    ßproc cmd input = ßprocGeneric cmd proc ($ input)
    øßproc = flip ßproc ø; ßøproc = øßproc
    øproc cmd args = proc cmd args ø
    æproc cmd input = æprocGeneric cmd proc ($ input)
    øæproc cmd = æproc cmd ø; æøproc = øæproc

    ßprocs cmd input = ßprocGeneric cmd procs ($ input)
    øßprocs = flip ßprocs ø; ßøprocs = øßprocs
    øprocs cmd args = procs cmd args ø
    æprocs cmd input = æprocGeneric cmd procs ($ input)
    øæprocs cmd = æprocs cmd ø; æøprocs = øæprocs

    ßinproc cmd input = ßprocGeneric cmd inproc ($ input)
    øßinproc = flip ßinproc ø; ßøinproc = øßinproc
    øinproc cmd args = inproc cmd args ø
    æinproc cmd input = æprocGeneric cmd inproc ($ input)
    øæinproc cmd = æinproc cmd ø; æøinproc = øæinproc

    ßinprocWithErr cmd input = ßprocGeneric cmd inprocWithErr ($ input)
    øßinprocWithErr = flip ßinprocWithErr ø; ßøinprocWithErr = øßinprocWithErr
    øinprocWithErr cmd args = inprocWithErr cmd args ø
    æinprocWithErr cmd input = æprocGeneric cmd inprocWithErr ($ input)
    øæinprocWithErr cmd = æinprocWithErr cmd ø; æøinprocWithErr = øæinprocWithErr

    ßprocStrict cmd input = ßprocGeneric cmd procStrict ($ input)
    øßprocStrict = flip ßprocStrict ø; ßøprocStrict = øßprocStrict
    øprocStrict cmd args = procStrict cmd args ø
    æprocStrict cmd input = æprocGeneric cmd procStrict ($ input)
    øæprocStrict cmd = æprocStrict cmd ø; æøprocStrict = øæprocStrict

    ßprocStrictWithErr cmd input = ßprocGeneric cmd procStrictWithErr ($ input)
    øßprocStrictWithErr = flip ßprocStrictWithErr ø; ßøprocStrictWithErr = øßprocStrictWithErr
    øprocStrictWithErr cmd args = procStrictWithErr cmd args ø
    æprocStrictWithErr cmd input = æprocGeneric cmd procStrictWithErr ($ input)
    øæprocStrictWithErr cmd = æprocStrictWithErr cmd ø; æøprocStrictWithErr = øæprocStrictWithErr

    µßproc cmd input = ßprocGeneric cmd Bytes.proc ($ input); ßµproc = µßproc
    øµßproc = flip µßproc ø; µøßproc = øµßproc; ßµøproc = øµßproc; µßøproc = øµßproc; ßøµproc = øµßproc; øßµproc = øµßproc
    øµproc cmd args = Bytes.proc cmd args ø; µøproc = øµproc
    æµproc cmd input = æprocGeneric cmd Bytes.proc ($ input); µæproc = æµproc
    øæµproc cmd = æµproc cmd ø; æøµproc = øæµproc; µæøproc = øæµproc; æµøproc = øæµproc; µøæproc = øæµproc; øµæproc = øæµproc

    µßprocs cmd input = ßprocGeneric cmd Bytes.procs ($ input); ßµprocs = µßprocs
    øµßprocs = flip µßprocs ø; µøßprocs = øµßprocs; ßµøprocs = øµßprocs; µßøprocs = øµßprocs; ßøµprocs = øµßprocs; øßµprocs = øµßprocs
    øµprocs cmd args = Bytes.procs cmd args ø; µøprocs = øµprocs
    æµprocs cmd input = æprocGeneric cmd Bytes.procs ($ input); µæprocs = æµprocs
    øæµprocs cmd = æµprocs cmd ø; æøµprocs = øæµprocs; µæøprocs = øæµprocs; æµøprocs = øæµprocs; µøæprocs = øæµprocs; øµæprocs = øæµprocs

    -- ByteString/“µ” variants

    µßinproc cmd input = ßprocGeneric cmd Bytes.inproc ($ input); ßµinproc = µßinproc
    øµßinproc = flip µßinproc ø; µøßinproc = øµßinproc; ßµøinproc = øµßinproc; µßøinproc = øµßinproc; ßøµinproc = øµßinproc; øßµinproc = øµßinproc
    øµinproc cmd args = Bytes.inproc cmd args ø; µøinproc = øµinproc
    æµinproc cmd input = æprocGeneric cmd Bytes.inproc ($ input); µæinproc = æµinproc
    øæµinproc cmd = æµinproc cmd ø; æøµinproc = øæµinproc; µæøinproc = øæµinproc; æµøinproc = øæµinproc; µøæinproc = øæµinproc; øµæinproc = øæµinproc

    µßinprocWithErr cmd input = ßprocGeneric cmd Bytes.inprocWithErr ($ input); ßµinprocWithErr = µßinprocWithErr
    øµßinprocWithErr = flip µßinprocWithErr ø; µøßinprocWithErr = øµßinprocWithErr; ßµøinprocWithErr = øµßinprocWithErr; µßøinprocWithErr = øµßinprocWithErr; ßøµinprocWithErr = øµßinprocWithErr; øßµinprocWithErr = øµßinprocWithErr
    øµinprocWithErr cmd args = Bytes.inprocWithErr cmd args ø; µøinprocWithErr = øµinprocWithErr
    æµinprocWithErr cmd input = æprocGeneric cmd Bytes.inprocWithErr ($ input); µæinprocWithErr = æµinprocWithErr
    øæµinprocWithErr cmd = æµinprocWithErr cmd ø; æøµinprocWithErr = øæµinprocWithErr; µæøinprocWithErr = øæµinprocWithErr; æµøinprocWithErr = øæµinprocWithErr; µøæinprocWithErr = øæµinprocWithErr; øµæinprocWithErr = øæµinprocWithErr

    µßprocStrict cmd input = ßprocGeneric cmd Bytes.procStrict ($ input); ßµprocStrict = µßprocStrict
    øµßprocStrict = flip µßprocStrict ø; µøßprocStrict = øµßprocStrict; ßµøprocStrict = øµßprocStrict; µßøprocStrict = øµßprocStrict; ßøµprocStrict = øµßprocStrict; øßµprocStrict = øµßprocStrict
    øµprocStrict cmd args = Bytes.procStrict cmd args ø; µøprocStrict = øµprocStrict
    æµprocStrict cmd input = æprocGeneric cmd Bytes.procStrict ($ input); µæprocStrict = æµprocStrict
    øæµprocStrict cmd = æµprocStrict cmd ø; æøµprocStrict = øæµprocStrict; µæøprocStrict = øæµprocStrict; æµøprocStrict = øæµprocStrict; µøæprocStrict = øæµprocStrict; øµæprocStrict = øæµprocStrict

    µßprocStrictWithErr cmd input = ßprocGeneric cmd Bytes.procStrictWithErr ($ input); ßµprocStrictWithErr = µßprocStrictWithErr
    øµßprocStrictWithErr = flip µßprocStrictWithErr ø; µøßprocStrictWithErr = øµßprocStrictWithErr; ßµøprocStrictWithErr = øµßprocStrictWithErr; µßøprocStrictWithErr = øµßprocStrictWithErr; ßøµprocStrictWithErr = øµßprocStrictWithErr; øßµprocStrictWithErr = øµßprocStrictWithErr
    øµprocStrictWithErr cmd args = Bytes.procStrictWithErr cmd args ø; µøprocStrictWithErr = øµprocStrictWithErr
    æµprocStrictWithErr cmd input = æprocGeneric cmd Bytes.procStrictWithErr ($ input); µæprocStrictWithErr = æµprocStrictWithErr
    øæµprocStrictWithErr cmd = æµprocStrictWithErr cmd ø; æøµprocStrictWithErr = øæµprocStrictWithErr; µæøprocStrictWithErr = øæµprocStrictWithErr; æµøprocStrictWithErr = øæµprocStrictWithErr; µøæprocStrictWithErr = øæµprocStrictWithErr; øµæprocStrictWithErr = øæµprocStrictWithErr

    -- Extra helpers

    -- Kind of an equivalent to `reduce Control.Foldl.list`
    shellList :: MonadIO m ⇒ Shell a → m [a]
    shellList = reduce (Fold (flip (:)) [] id)
    :}

    :{
    -- “lsif” but only for current level
    lsif1 mp = (ls >=> \x → (x,) · mp x) × mfilter snd × fmap fst
    :}

    -- Aliases
    :{
    -- Directory listing aliases
    l args = procs "ls" (["--color=auto", "-lah"] ‰ args) ø
    øl = l ø
    ll args = procs "ls" (["--color=auto", "-lAh"] ‰ args) ø
    øll = ll ø

    -- Vim aliases
    v = procs "nvim"
    øv = v ø -- No args
    øøv = v ø ø -- No args and no stdin

    -- Git aliases
    _gitæ = T.words ("git -c color.ui=always")
    _git = T.unwords _gitæ ‰ " "
    gits = øßinproc (_git ‰ "status")
    gitl = øßinproc (_git ‰ "log")
    gitl1 = øßinproc (_git ‰ "log -1")
    gitl2 = øßinproc (_git ‰ "log -2")
    gitl3 = øßinproc (_git ‰ "log -3")
    gitl4 = øßinproc (_git ‰ "log -4")
    gitl5 = øßinproc (_git ‰ "log -5")
    gitls = øßinproc (_git ‰ "log --show-signature")
    gitls1 = øßinproc (_git ‰ "log --show-signature -1")
    gitls2 = øßinproc (_git ‰ "log --show-signature -2")
    gitls3 = øßinproc (_git ‰ "log --show-signature -3")
    gitls4 = øßinproc (_git ‰ "log --show-signature -4")
    gitls5 = øßinproc (_git ‰ "log --show-signature -5")
    gitc = øßinproc (_git ‰ "commit")
    gitca = øßinproc (_git ‰ "commit --amend")
    gitcs = øßinproc (_git ‰ "commit -S")
    gitcsa = øßinproc (_git ‰ "commit -S --amend")
    gitcas = øßinproc (_git ‰ "commit --amend -S")
    gitcm = øßinproc (_git ‰ "commit -m")
    gitcam = øßinproc (_git ‰ "commit --amend -m")
    gitcsm = øßinproc (_git ‰ "commit -S -m")
    gitcsam = øßinproc (_git ‰ "commit -S --amend -m")
    gitcasm = øßinproc (_git ‰ "commit --amend -S -m")
    -- just add a signature to a commit (last commit by default)
    gitsign args = øæinproc (T.words (_git ‰ "commit -S --amend --no-edit") ‰ args)
    gitsignß = gitsign . T.words
    -- rebase + sign. e.g. `gitrsign "origin/master"`.
    -- it will do a rebase but also sign all the rebased commits.
    gitrsign args = øæinproc ((T.words (_git ‰ "rebase -i --exec") ‰ [_git ‰ "commit -S --amend --no-edit"]) ‰ args)
    gita = øßinproc (_git ‰ "add")
    gitd = øßinproc (_git ‰ "diff")
    gitds = øßinproc (_git ‰ "diff --staged")
    gitb = øßinproc ("git branch") & grep (begins "* ") & sedEntire ("* " *> chars) & single
    ßgitb = gitb × lineToText
    gitbn = øßinproc (_git ‰ "branch")
    gitco = øßinproc (_git ‰ "checkout")
    gitpl = øæinproc . ((T.words (_git ‰ "pull origin --")) ‰) =<< fmap pure ßgitb
    gitph = øæinproc . ((T.words (_git ‰ "push origin --")) ‰) =<< fmap pure ßgitb

    -- shred with my favorite options
    shreddy args = øæinproc (T.words "shred -vufz -n10" ‰ args); shreddyß = shreddy . T.words
    :}
  '';

  colorizedGhciScript = color:
    assert builtins.elem color ["red" "green"];
    let
      colorize = c: s:
        assert builtins.isInt c;
        assert builtins.isString s;
        ''\ESC[${toString c}m\STX${s}\ESC[m\STX'';
    in
    writeText "colorized-hell-ghci-script-${color}" ''
      ${ghciScript}
      :set prompt "${colorize (if color == "red" then 31 else 32) "λ"} "
    '';

  hell = writeShellApplication {
    name = "hell";
    runtimeInputs = [ ghc ];

    text = ''
      if (( UID == 0 ))
      then file=${esc (colorizedGhciScript "red")}
      else file=${esc (colorizedGhciScript "green")}
      fi

      ${lib.escapeShellArgs [
        "ghci"
        "-ignore-dot-ghci"
        "-ghci-script"
      ]} "$file" "$@"
    '';
  };
in

hell
