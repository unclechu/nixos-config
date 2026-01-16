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

import Prelude hiding (id, (.), FilePath, head, tail, last)
import qualified Prelude

import Data.Function hiding (id, (.))
import Data.Functor

import Numeric (showFFloat)
import Numeric.Natural
import Data.Ratio

import Data.Proxy
import Data.Kind
import Data.Maybe
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
import qualified Data.Bifunctor as BF

import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J

import qualified Data.Time as Time

import Control.Arrow
import Control.Category hiding (first)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Control.Monad.Managed as Managed
import qualified Control.Concurrent.Async as Async

import System.Environment
import System.Directory
import qualified System.Process as SysProc
import qualified System.Posix.Process as Unix
import qualified System.IO as SysIO

import Turtle hiding ((<&>), (%), l, toText, cd)
import qualified Turtle
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
-- ə — Finnish layout: AltGr + ə

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

-- “þ” suffix means the output is intended for piping (`Shell Line`)
--     while functions that do not have it forward the output to stdout
--     automatically (`IO`-compatible, can be run without manual redirects).
--     For example `gitsþ & stdout` vs. `gits`.

-- “ə” prefix means the commands takes standard input.
--     It is used only for commands for which standard input piping
--     is uncommon/rare use case.

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

-- Suck everything from a Shell into a list
suck ∷ MonadIO m ⇒ Shell a → m [a]; suck = reduce Fold.list
-- Reversed `suck`
suckRev ∷ MonadIO m ⇒ Shell a → m [a]; suckRev = reduce (Fold (flip (:)) [] id)

class ToText a where toText ∷ a → T.Text
instance ToText T.Text where toText = id
instance ToText String where toText = T.pack
instance ToText BSC.ByteString where toText = T.decodeUtf8

feedStr ∷ ToText s ⇒ Shell s → Shell Line; feedStr = (>>= select . textToLines . toText)

-- * Extra Turtle variants

-- “lsif” but only for current level
lsif1 mp = (ls >=> \x → (x,) · mp x) × mfilter snd × fmap fst

-- * Turtle extras

-- ** Formatters

-- Like `Turtle.f` formatter for fractional numbers but with custom amount of digits precision after the dot
ff ∷ Integral p ⇒ p → Format r (Double → r); ff p = makeFormat (\n → T.pack (showFFloat ((Just . fromIntegral) p) n ""))

-- ** Extra filters

tailN n = reduce (Fold.lastN (fromIntegral n)) >=> select; tail = tailN 10
headN n = limit (fromIntegral n); head = headN 10
last = reduce Fold.last
first = reduce Fold.head

-- ** Extra spawners

-- Helpers for /dev/null redirects
withNullR = liftIO . SysIO.withFile "/dev/null" SysIO.ReadMode
withNullW = liftIO . SysIO.withFile "/dev/null" SysIO.AppendMode
withNullRnW f = withNullR $ \r → withNullW $ \w → f (r, w)

-- Silently spawn and forget a subprocess (it will lives after the Hell session is done)
burp (cmd ∷ Text) (args ∷ [Text]) = withNullRnW $ \(r, w) → void $ SysProc.createProcess (SysProc.proc (T.unpack cmd) (fmap T.unpack args)) { SysProc.std_in = SysProc.UseHandle r, SysProc.std_out = SysProc.UseHandle w, SysProc.std_err = SysProc.UseHandle w, SysProc.new_session = True }
æburp (cmd ∷ [Text]) = NE.nonEmpty cmd & maybe (fail "Empty command") (unconsNE × uncurry burp)
ßburp = æburp . T.words

-- * Aliases

-- ** Directory listing aliases
_læ = T.words "ls --color=auto -lah"; _llæ = T.words "ls --color=auto -lAh"
læ args = øæprocs (_læ ‰ args); lß = læ . T.words; l = læ ø
læþ args = øæinproc (_læ ‰ args); lßþ = læþ . T.words; lþ = læþ ø
llæ args = øæprocs (_llæ ‰ args); llß = llæ . T.words; ll = llæ ø
llæþ args = øæinproc (_llæ ‰ args); llßþ = llæþ . T.words; llþ = llæþ ø

-- ** Vim aliases
əvæ args = æprocs (["nvim"] ‰ args); væ = flip əvæ ø
əvß args = əvæ (T.words args); vß = flip əvß ø
əv = əvæ ø; v = əv ø

-- ** Git aliases

-- Git status
_gits = "git status"; _gitsæ = T.words _gits
gitsæ args = øæprocs (_gitsæ ‰ args); gitsæþ args = øæinproc (_gitsæ ‰ args)
gitsß = gitsæ . T.words; gitsßþ = gitsæþ . T.words
gits = gitsæ ø; gitsþ = gitsæþ ø
-- Git log
_gitl = "git log"; _gitlæ = T.words _gitl
gitlæ args = øæprocs (_gitlæ ‰ args); gitlæþ args = øæinproc (_gitlæ ‰ args)
gitlß = gitlæ . T.words; gitlßþ = gitlæþ . T.words
gitl = gitlæ ø; gitlþ = gitlæþ ø
-- Git log, limit to 1
_gitl1æ = ["-1"]
gitl1æ args = gitlæ (_gitl1æ ‰ args); gitl1æþ args = gitlæþ (_gitl1æ ‰ args)
gitl1ß = gitl1æ . T.words; gitl1ßþ = gitl1æþ . T.words
gitl1 = gitl1æ ø; gitl1þ = gitl1æþ ø
-- Git log, limit to 2
_gitl2æ = ["-2"]
gitl2æ args = gitlæ (_gitl2æ ‰ args); gitl2æþ args = gitlæþ (_gitl2æ ‰ args)
gitl2ß = gitl2æ . T.words; gitl2ßþ = gitl2æþ . T.words
gitl2 = gitl2æ ø; gitl2þ = gitl2æþ ø
-- Git log, limit to 3
_gitl3æ = ["-3"]
gitl3æ args = gitlæ (_gitl3æ ‰ args); gitl3æþ args = gitlæþ (_gitl3æ ‰ args)
gitl3ß = gitl3æ . T.words; gitl3ßþ = gitl3æþ . T.words
gitl3 = gitl3æ ø; gitl3þ = gitl3æþ ø
-- Git log, limit to 4
_gitl4æ = ["-4"]
gitl4æ args = gitlæ (_gitl4æ ‰ args); gitl4æþ args = gitlæþ (_gitl4æ ‰ args)
gitl4ß = gitl4æ . T.words; gitl4ßþ = gitl4æþ . T.words
gitl4 = gitl4æ ø; gitl4þ = gitl4æþ ø
-- Git log, limit to 5
_gitl5æ = ["-5"]
gitl5æ args = gitlæ (_gitl5æ ‰ args); gitl5æþ args = gitlæþ (_gitl5æ ‰ args)
gitl5ß = gitl5æ . T.words; gitl5ßþ = gitl5æþ . T.words
gitl5 = gitl5æ ø; gitl5þ = gitl5æþ ø
-- Git log with showing signature
_gitlsæ = ["--show-signature"]
gitlsæ args = gitlæ (_gitlsæ ‰ args); gitlsæþ args = gitlæþ (_gitlsæ ‰ args)
gitlsß = gitlsæ . T.words; gitlsßþ = gitlsæþ . T.words
gitls = gitlsæ ø; gitlsþ = gitlsæþ ø
-- Git log with showing signature, limit to 1
gitls1æ args = gitlsæ (_gitl1æ ‰ args); gitls1æþ args = gitlsæþ (_gitl1æ ‰ args)
gitls1ß = gitls1æ . T.words; gitls1ßþ = gitls1æþ . T.words
gitls1 = gitls1æ ø; gitls1þ = gitls1æþ ø
-- Git log with showing signature, limit to 2
gitls2æ args = gitlsæ (_gitl2æ ‰ args); gitls2æþ args = gitlsæþ (_gitl2æ ‰ args)
gitls2ß = gitls2æ . T.words; gitls2ßþ = gitls2æþ . T.words
gitls2 = gitls2æ ø; gitls2þ = gitls2æþ ø
-- Git log with showing signature, limit to 3
gitls3æ args = gitlsæ (_gitl3æ ‰ args); gitls3æþ args = gitlsæþ (_gitl3æ ‰ args)
gitls3ß = gitls3æ . T.words; gitls3ßþ = gitls3æþ . T.words
gitls3 = gitls3æ ø; gitls3þ = gitls3æþ ø
-- Git log with showing signature, limit to 4
gitls4æ args = gitlsæ (_gitl4æ ‰ args); gitls4æþ args = gitlsæþ (_gitl4æ ‰ args)
gitls4ß = gitls4æ . T.words; gitls4ßþ = gitls4æþ . T.words
gitls4 = gitls4æ ø; gitls4þ = gitls4æþ ø
-- Git log with showing signature, limit to 5
gitls5æ args = gitlsæ (_gitl5æ ‰ args); gitls5æþ args = gitlsæþ (_gitl5æ ‰ args)
gitls5ß = gitls5æ . T.words; gitls5ßþ = gitls5æþ . T.words
gitls5 = gitls5æ ø; gitls5þ = gitls5æþ ø

-- Git commit
_gitc = "git commit"; _gitcæ = T.words _gitc
gitcæ args = øæprocs (_gitcæ ‰ args); gitcæþ args = øæinproc (_gitcæ ‰ args)
gitcß = gitcæ . T.words; gitcßþ = gitcæþ . T.words
gitc = gitcæ ø; gitcþ = gitcæþ ø
-- Git commit amend
_gitcaæ = ["--amend"]
gitcaæ args = gitcæ (_gitcaæ ‰ args); gitcaæþ args = gitcæþ (_gitcaæ ‰ args)
gitcaß = gitcaæ . T.words; gitcaßþ = gitcaæþ . T.words
gitca = gitcaæ ø; gitcaþ = gitcaæþ ø
-- Git commit sign
_gitcsæ = ["-S"]
gitcsæ args = gitcæ (_gitcsæ ‰ args); gitcsæþ args = gitcæþ (_gitcsæ ‰ args)
gitcsß = gitcsæ . T.words; gitcsßþ = gitcsæþ . T.words
gitcs = gitcsæ ø; gitcsþ = gitcsæþ ø
-- Git commit sign amend
gitcsaæ args = gitcsæ (_gitcaæ ‰ args); gitcsaæþ args = gitcsæþ (_gitcaæ ‰ args)
gitcsaß = gitcsaæ . T.words; gitcsaßþ = gitcsaæþ . T.words
gitcsa = gitcsaæ ø; gitcsaþ = gitcsaæþ ø
-- Git commit amend sign
gitcasæ args = gitcaæ (_gitcsæ ‰ args); gitcasæþ args = gitcaæþ (_gitcsæ ‰ args)
gitcasß = gitcasæ . T.words; gitcasßþ = gitcasæþ . T.words
gitcas = gitcasæ ø; gitcasþ = gitcasæþ ø
-- Git commit message
_gitcmæ = ["-m"]
gitcmæ args = gitcæ (_gitcmæ ‰ args); gitcmæþ args = gitcæþ (_gitcmæ ‰ args)
gitcmß = gitcmæ . T.words; gitcmßþ = gitcmæþ . T.words
-- No arguments after “-m” makes no sense
-- gitcm = gitcmæ ø; gitcmþ = gitcmæþ ø
-- Git commit amend message
gitcamæ args = gitcaæ (_gitcmæ ‰ args); gitcamæþ args = gitcaæþ (_gitcmæ ‰ args)
gitcamß = gitcamæ . T.words; gitcamßþ = gitcamæþ . T.words
-- No arguments after “-m” makes no sense
-- gitcam = gitcamæ ø; gitcamþ = gitcamæþ ø
-- Git commit sign message
gitcsmæ args = gitcsæ (_gitcmæ ‰ args); gitcsmæþ args = gitcaæþ (_gitcmæ ‰ args)
gitcsmß = gitcsmæ . T.words; gitcsmßþ = gitcsmæþ . T.words
-- No arguments after “-m” makes no sense
-- gitcsm = gitcsmæ ø; gitcsmþ = gitcsmæþ ø
-- Git commit sign amend message
gitcsamæ args = gitcsaæ (_gitcmæ ‰ args); gitcsamæþ args = gitcsaæþ (_gitcmæ ‰ args)
gitcsamß = gitcsamæ . T.words; gitcsamßþ = gitcsamæþ . T.words
-- No arguments after “-m” makes no sense
-- gitcsam = gitcsamæ ø; gitcsamþ = gitcsamæþ ø
-- Git commit amend sign message
gitcasmæ args = gitcasæ (_gitcmæ ‰ args); gitcasmæþ args = gitcasæþ (_gitcmæ ‰ args)
gitcasmß = gitcasmæ . T.words; gitcasmßþ = gitcasmæþ . T.words
-- No arguments after “-m” makes no sense
-- gitcasm = gitcasmæ ø; gitcasmþ = gitcasmæþ ø

-- just add a signature to a commit (last commit by default)
_gitneæ = ["--no-edit"]
gitsignæ args = gitcsaæ (_gitneæ ‰ args); gitsignæþ args = gitcsaæþ (_gitneæ ‰ args)
gitsignß = gitsignæ . T.words; gitsignßþ = gitsignæþ . T.words
gitsign = gitsignæ ø; gitsignþ = gitsignæþ ø

-- rebase + sign. e.g. `gitrsignß "origin/master"`.
-- it will do a rebase but also sign all the rebased commits.
_gitrsignæ = T.words "git rebase -i --exec" ‰ ["git commit -S --amend --no-edit"]
gitrsignæ args = øæprocs (_gitrsignæ ‰ args); gitrsignæþ args = øæinproc (_gitrsignæ ‰ args)
gitrsignß = gitrsignæ . T.words; gitrsignßþ = gitrsignæþ . T.words
gitrsign = gitrsignæ ø; gitrsignþ = gitrsignæþ ø

-- Git add
_gita = "git add"; _gitaæ = T.words _gita
gitaæ args = øæprocs (_gitaæ ‰ args); gitaæþ args = øæinproc (_gitaæ ‰ args)
gitaß = gitaæ . T.words; gitaßþ = gitaæþ . T.words
gita = gitaæ ["."]; gitaþ = gitaæþ ["."]

-- Git diff
_gitd = "git diff"; _gitdæ = T.words _gitd
gitdæ args = øæprocs (_gitdæ ‰ args); gitdæþ args = øæinproc (_gitdæ ‰ args)
gitdß = gitdæ . T.words; gitdßþ = gitdæþ . T.words
gitd = gitdæ ø; gitdþ = gitdæþ ø
-- Git diff staged
_gitdsæ = ["--staged"]
gitdsæ args = gitdæ (_gitdsæ ‰ args); gitdsæþ args = gitdæþ (_gitdsæ ‰ args)
gitdsß = gitdsæ . T.words; gitdsßþ = gitdsæþ . T.words
gitds = gitdsæ ø; gitdsþ = gitdsæþ ø

gitb = øßinproc "git branch" & grep (begins "* ") & sedEntire ("* " *> chars) & single
ßgitb = gitb × lineToText

-- Git branch
_gitbn = "git branch"; _gitbnæ = T.words _gitbn
gitbnæ args = øæprocs (_gitbnæ ‰ args); gitbnæþ args = øæinproc (_gitbnæ ‰ args)
gitbnß = gitbnæ . T.words; gitbnßþ = gitbnæþ . T.words
gitbn = gitbnæ ø; gitbnþ = gitbnæþ ø

-- Git checkout
_gitco = "git checkout"; _gitcoæ = T.words _gitco
gitcoæ args = øæprocs (_gitcoæ ‰ args); gitcoæþ args = øæinproc (_gitcoæ ‰ args)
gitcoß = gitcoæ . T.words; gitcoßþ = gitcoæþ . T.words
gitco = gitcoæ ø; gitcoþ = gitcoæþ ø

-- Git pull/push to/from “origin” current branch
gitpl = øæprocs . (T.words "git pull origin --" ‰) . pure =<< ßgitb
gitph = øæprocs . (T.words "git push origin --" ‰) . pure =<< ßgitb

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

_isActiveTmuxPane pane = øæinproc (T.words "tmux display-message -p -F #{pane_active} -t" <> [pane]) & single & fmap (== "1")
_tmuxReportCurrentPaneCwd pane = _isActiveTmuxPane pane >>= \x → ((guard x >>) · liftIO (findExecutable "tmux-report-current-pane-cwd")) >>= maybe (pure ()) (sh . øæinproc . pure . T.pack)
-- `Turtle.cd` but report working directory for tmux (related to my config specifically)
cd ∷ (MonadIO m, MonadFail m) ⇒ FilePath → m (); cd x = Turtle.cd x >> (((>>) · need "TMUX" ° need "TMUX_PANE") >>= maybe (pure ()) _tmuxReportCurrentPaneCwd)

-- “cd” up N times, `up 3` means `cd ../../../`
up n = cd $ DF.fold (replicate n "../")

-- Create directory and “cd" to it
mkdircd dir = mkdir dir >> cd dir
mktreecd dir = mktree dir >> mktree dir

-- Note that this history is not up-to-date, current session commands are written
-- to the history only as soon as the session is done.
historyþ = home >>= \homeDir → input (homeDir ‰ "/.ghc/ghci_history")
history = historyþ & stdout
æhistory = historyþ & reduce Fold.list

-- * Miscellaneous stuff

main = pure () -- A dummy plug to avoid interactive GHC file validation failure
