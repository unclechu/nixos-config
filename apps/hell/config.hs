-- Wenzel’s Hell the Haskell shell configuration.
--
-- Note that this file is made compatible with vanilla Haskell while its used
-- as a GHCi script which has its own limitations. For example you can’t define
-- type and definition on separate lines without using `:{` and `:}` which would
-- break compatibility with vanilla Haskell. So for this reason all explicitly
-- typed functions have to have both type and definition on the same line
-- (separated by semicolon `;`). And in general there must be no multiline
-- functions.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

{-# LANGUAGE GHC2024, UnicodeSyntax #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving, DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

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

import qualified Data.Foldable as DF

import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J

import qualified Data.Time as Time

import Control.Arrow
import Control.Category
import Control.Monad

import System.Environment
import System.Directory
import qualified System.Process as SysProc
import qualified System.Posix.Process as Unix
import qualified System.IO as SysIO

import Turtle hiding ((<&>), (%), l)
import qualified Turtle.Bytes as Bytes

-- * Unicode operators
-- These unicode symbols can be typed using Finnish keyboard layout.

-- Finnish layout: AltGr + ö
ø ∷ Monoid m ⇒ m; ø = mempty
-- Finnish layout: AltGr + 5
(‰) ∷ Semigroup a ⇒ a → a → a; (‰) = (<>)
-- Finnish layout: AltGr + Shift + x
(·) ∷ Functor f ⇒ (a → b) → f a → f b; (·) = (<$>)
-- Finnish layout: AltGr + x
(×) ∷ Functor f ⇒ f a → (a → b) → f b; (×) = (<&>)
-- Finnish layout: AltGr + Shift + 0
(°) ∷ Applicative f ⇒ f (a → b) → f a → f b; (°) = (<*>)
-- Finnish layout: AltGr + Shift + +
(¿) ∷ Alternative f ⇒ f a → f a → f a; (¿) = (<|>)

-- * List helpers

unconsNE ∷ NE.NonEmpty a → (a, [a]); unconsNE (x NE.:| xs) = (x, xs)

-- * Turtle sub-process commands

-- ø — Finnish layout: AltGr + ö
-- æ — Finnish layout: AltGr + ä
-- ß — Finnish layout: AltGr + s
-- µ — Finnish layout: AltGr + m
-- þ — Finnish layout: AltGr + t

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

-- “þ” prefix means the output is automatically redirected to stdout.
--     For example `gitsþ` vs. `gits & stdout`.

æprocGeneric (cmd ∷ [Text]) f applyF = NE.nonEmpty cmd & maybe (fail "Empty command") (unconsNE × uncurry f × applyF)
ßprocGeneric (cmd ∷ Text) = æprocGeneric (T.words cmd)

-- ** Text variants

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

-- ** ByteString/“µ” variants

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

-- ** Extra helpers

-- Kind of an equivalent to `reduce Control.Foldl.list`
shellList :: MonadIO m ⇒ Shell a → m [a]; shellList = reduce (Fold (flip (:)) [] id)

-- * Extra Turtle variants

-- “lsif” but only for current level
lsif1 mp = (ls >=> \x → (x,) · mp x) × mfilter snd × fmap fst

-- * Aliases

-- ** Directory listing aliases
l args = procs "ls" (["--color=auto", "-lah"] ‰ args) ø
øl = l ø
ll args = procs "ls" (["--color=auto", "-lAh"] ‰ args) ø
øll = ll ø

-- ** Vim aliases
v = procs "nvim"
øv = v ø -- No args
vø = øv
øøv = v ø ø -- No args and no stdin
øvø = v ø ø -- No args and no stdin

-- ** Git aliases

_gitæ = T.words "git -c color.ui=always"
_git = T.unwords _gitæ ‰ " "

-- Git status
gitsæ args = øæinproc (_gitæ ‰ ["status"] ‰ args); gitsæþ = stdout . gitsæ
gitsß = gitsæ . T.words; gitsßþ = stdout . gitsß
gits = gitsæ ø; gitsþ = gits & stdout

-- Git log
gitlæ args = øæinproc (_gitæ ‰ ["log"] ‰ args); gitlæþ = stdout . gitlæ
gitlß = gitlæ . T.words; gitlßþ = stdout . gitlß
gitl = gitlæ ø; gitlþ = gitl & stdout
-- Git log, limit to 1
gitl1æ args = gitlæ (["-1"] ‰ args); gitl1æþ = stdout . gitl1æ
gitl1ß = gitl1æ . T.words; gitl1ßþ = stdout . gitl1ß
gitl1 = gitl1æ ø; gitl1þ = gitl1 & stdout
-- Git log, limit to 2
gitl2æ args = gitlæ (["-2"] ‰ args); gitl2æþ = stdout . gitl2æ
gitl2ß = gitl2æ . T.words; gitl2ßþ = stdout . gitl2ß
gitl2 = gitl2æ ø; gitl2þ = gitl2 & stdout
-- Git log, limit to 3
gitl3æ args = gitlæ (["-3"] ‰ args); gitl3æþ = stdout . gitl3æ
gitl3ß = gitl3æ . T.words; gitl3ßþ = stdout . gitl3ß
gitl3 = gitl3æ ø; gitl3þ = gitl3 & stdout
-- Git log, limit to 4
gitl4æ args = gitlæ (["-4"] ‰ args); gitl4æþ = stdout . gitl4æ
gitl4ß = gitl4æ . T.words; gitl4ßþ = stdout . gitl4ß
gitl4 = gitl4æ ø; gitl4þ = gitl4 & stdout
-- Git log, limit to 5
gitl5æ args = gitlæ (["-5"] ‰ args); gitl5æþ = stdout . gitl5æ
gitl5ß = gitl5æ . T.words; gitl5ßþ = stdout . gitl5ß
gitl5 = gitl5æ ø; gitl5þ = gitl5 & stdout
-- Git log with showing signature
gitlsæ args = gitlæ (["--show-signature"] ‰ args); gitlsæþ = stdout . gitlsæ
gitlsß = gitlsæ . T.words; gitlsßþ = stdout . gitlsß
gitls = gitlsæ ø; gitlsþ = gitls & stdout
-- Git log with showing signature, limit to 1
gitls1æ args = gitlsæ (["-1"] ‰ args); gitls1æþ = stdout . gitls1æ
gitls1ß = gitls1æ . T.words; gitls1ßþ = stdout . gitls1ß
gitls1 = gitls1æ ø; gitls1þ = gitls1 & stdout
-- Git log with showing signature, limit to 2
gitls2æ args = gitlsæ (["-2"] ‰ args); gitls2æþ = stdout . gitls2æ
gitls2ß = gitls2æ . T.words; gitls2ßþ = stdout . gitls2ß
gitls2 = gitls2æ ø; gitls2þ = gitls2 & stdout
-- Git log with showing signature, limit to 3
gitls3æ args = gitlsæ (["-3"] ‰ args); gitls3æþ = stdout . gitls3æ
gitls3ß = gitls3æ . T.words; gitls3ßþ = stdout . gitls3ß
gitls3 = gitls3æ ø; gitls3þ = gitls3 & stdout
-- Git log with showing signature, limit to 4
gitls4æ args = gitlsæ (["-4"] ‰ args); gitls4æþ = stdout . gitls4æ
gitls4ß = gitls4æ . T.words; gitls4ßþ = stdout . gitls4ß
gitls4 = gitls4æ ø; gitls4þ = gitls4 & stdout
-- Git log with showing signature, limit to 5
gitls5æ args = gitlsæ (["-5"] ‰ args); gitls5æþ = stdout . gitls5æ
gitls5ß = gitls5æ . T.words; gitls5ßþ = stdout . gitls5ß
gitls5 = gitls5æ ø; gitls5þ = gitls5 & stdout

-- Git commit
gitcæ args = øæinproc (_gitæ ‰ ["commit"] ‰ args); gitcæþ = stdout . gitcæ
gitcß = gitcæ . T.words; gitcßþ = stdout . gitcß
gitc = gitcæ ø; gitcþ = gitc & stdout
-- Git commit amend
gitcaæ args = gitcæ (["--amend"] ‰ args); gitcaæþ = stdout . gitcaæ
gitcaß = gitcaæ . T.words; gitcaßþ = stdout . gitcaß
gitca = gitcaæ ø; gitcaþ = gitca & stdout
-- Git commit sign
gitcsæ args = gitcæ (["-S"] ‰ args); gitcsæþ = stdout . gitcsæ
gitcsß = gitcsæ . T.words; gitcsßþ = stdout . gitcsß
gitcs = gitcsæ ø; gitcsþ = gitcs & stdout
-- Git commit sign amend
gitcsaæ args = gitcsæ (["--amend"] ‰ args); gitcsaæþ = stdout . gitcsaæ
gitcsaß = gitcsaæ . T.words; gitcsaßþ = stdout . gitcsaß
gitcsa = gitcsaæ ø; gitcsaþ = gitcsa & stdout
-- Git commit amend sign
gitcasæ args = gitcaæ (["-S"] ‰ args); gitcasæþ = stdout . gitcasæ
gitcasß = gitcasæ . T.words; gitcasßþ = stdout . gitcasß
gitcas = gitcasæ ø; gitcasþ = gitcas & stdout
-- Git commit message
gitcmæ args = gitcæ (["-m"] ‰ args); gitcmæþ = stdout . gitcmæ
gitcmß = gitcmæ . T.words; gitcmßþ = stdout . gitcmß
gitcm = gitcmæ ø; gitcmþ = gitcm & stdout
-- Git commit amend message
gitcamæ args = gitcaæ (["-m"] ‰ args); gitcamæþ = stdout . gitcamæ
gitcamß = gitcamæ . T.words; gitcamßþ = stdout . gitcamß
gitcam = gitcamæ ø; gitcamþ = gitcam & stdout
-- Git commit sign message
gitcsmæ args = gitcsæ (["-m"] ‰ args); gitcsmæþ = stdout . gitcsmæ
gitcsmß = gitcsmæ . T.words; gitcsmßþ = stdout . gitcsmß
gitcsm = gitcsmæ ø; gitcsmþ = gitcsm & stdout
-- Git commit sign amend message
gitcsamæ args = gitcsaæ (["-m"] ‰ args); gitcsamæþ = stdout . gitcsamæ
gitcsamß = gitcsamæ . T.words; gitcsamßþ = stdout . gitcsamß
gitcsam = gitcsamæ ø; gitcsamþ = gitcsam & stdout
-- Git commit amend sign message
gitcasmæ args = gitcasæ (["-m"] ‰ args); gitcasmæþ = stdout . gitcasmæ
gitcasmß = gitcasmæ . T.words; gitcasmßþ = stdout . gitcasmß
gitcasm = gitcasmæ ø; gitcasmþ = gitcasm & stdout

-- just add a signature to a commit (last commit by default)
gitsignæ args = øæinproc (T.words (_git ‰ "commit -S --amend --no-edit") ‰ args); gitsignæþ = stdout . gitsignæ
gitsignß = gitsignæ . T.words; gitsignßþ = stdout . gitsignß
gitsign = gitsignæ ø; gitsignþ = gitsign & stdout

-- rebase + sign. e.g. `gitrsignß "origin/master"`.
-- it will do a rebase but also sign all the rebased commits.
gitrsignæ args = øæinproc ((T.words (_git ‰ "rebase -i --exec") ‰ [_git ‰ "commit -S --amend --no-edit"]) ‰ args); gitrsignæþ = stdout . gitrsignæ
gitrsignß = gitrsignæ . T.words; gitrsignßþ = stdout . gitrsignß
gitrsign = gitrsignæ ø; gitrsignþ = gitrsign & stdout

-- Git add
gitaæ args = øæinproc (_gitæ ‰ ["add"] ‰ args); gitaæþ = stdout . gitaæ
gitaß = gitaæ . T.words; gitaßþ = stdout . gitaß
gita = gitaæ ø; gitaþ = gita & stdout

-- Git diff
gitdæ args = øæinproc (_gitæ ‰ ["diff"] ‰ args); gitdæþ = stdout . gitdæ
gitdß = gitdæ . T.words; gitdßþ = stdout . gitdß
gitd = gitdæ ø; gitdþ = gitd & stdout
-- Git diff staged
gitdsæ args = gitdæ (["--staged"] ‰ args); gitdsæþ = stdout . gitdsæ
gitdsß = gitdsæ . T.words; gitdsßþ = stdout . gitdsß
gitds = gitdsæ ø; gitdsþ = gitds & stdout

gitb = øßinproc "git branch" & grep (begins "* ") & sedEntire ("* " *> chars) & single
ßgitb = gitb × lineToText

-- Git branch
gitbnæ args = øæinproc (_gitæ ‰ ["branch"] ‰ args); gitbnæþ = stdout . gitbnæ
gitbnß = gitbnæ . T.words; gitbnßþ = stdout . gitbnß
gitbn = gitbnæ ø; gitbnþ = gitbn & stdout

-- Git checkout
gitcoæ args = øæinproc (_gitæ ‰ ["checkout"] ‰ args); gitcoæþ = stdout . gitcoæ
gitcoß = gitcoæ . T.words; gitcoßþ = stdout . gitcoß
gitco = gitcoæ ø; gitcoþ = gitco & stdout

-- Git pull/push to/from “origin” current branch
gitpl = øæinproc . (T.words (_git ‰ "pull origin --") ‰) . pure =<< ßgitb; gitplþ = gitpl & stdout
gitph = øæinproc . (T.words (_git ‰ "push origin --") ‰) . pure =<< ßgitb; gitphþ = gitph & stdout

-- ** shred with my favorite options
shreddy args = øæinproc (T.words "shred -vufz -n10" ‰ args)
shreddyß = shreddy . T.words
shreddyþ = stdout . shreddy
shreddyßþ = stdout . shreddyß

-- ** Path resolving

-- Resolve absolute path of an executable following all the symlinks on the way.
--
-- For example `p bash` will resolve to something like this:
--
-- /nix/store/rdd4pnr4x9rqc9wgbibhngv217w2xvxl-bash-interactive-5.2p26/bin/bash
p a = liftIO (findExecutable a) >>= maybe (fail $ "Failed to find " ‰ show a ‰ " in PATH") (\a → øæinproc ["readlink", "-f", "--", T.pack a] & single)
ßp a = p a × lineToText

-- ** Changing directories

-- “cd” up N times, `up 3` means `cd ../../../`
up n = cd $ DF.fold (replicate n "../")

-- Create directory and “cd" to it
mkdircd dir = mkdir dir >> cd dir
mktreecd dir = mktree dir >> mktree dir

-- Helpers for /dev/null redirects
withNullR = liftIO . SysIO.withFile "/dev/null" SysIO.ReadMode
withNullW = liftIO . SysIO.withFile "/dev/null" SysIO.AppendMode
withNullRnW f = withNullR $ \r → withNullW $ \w → f (r, w)

-- Silently spawn and forget a subprocess (it will lives after the Hell session is done)
burp (cmd ∷ Text) (args ∷ [Text]) = withNullRnW $ \(r, w) → void $ SysProc.createProcess (SysProc.proc (T.unpack cmd) (fmap T.unpack args)) { SysProc.std_in = SysProc.UseHandle r, SysProc.std_out = SysProc.UseHandle w, SysProc.std_err = SysProc.UseHandle w, SysProc.new_session = True }
æburp (cmd ∷ [Text]) = NE.nonEmpty cmd & maybe (fail "Empty command") (unconsNE × uncurry burp)
ßburp = æburp . T.words

-- Note that this history is not up-to-date, current session commands are written
-- to the history only as soon as the session is done.
ßhistory = home >>= \homeDir → liftIO (T.readFile (homeDir ‰ "/.ghc/ghci_history"))
æhistory = ßhistory × T.lines

-- * Miscellaneous stuff

main = pure () -- A dummy plug to avoid interactive GHC file validation failure
