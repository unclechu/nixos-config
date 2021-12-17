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

    p.containers
    p.mtl

    p.time
    p.aeson
    p.aeson-pretty
  ]);

  ghciScript = ''
    :set -XNoMonomorphismRestriction
    :set -XOverloadedStrings
    :set -XLambdaCase
    :set -XUnicodeSyntax

    :set -XDataKinds
    :set -XTypeFamilies
    :set -XTypeOperators
    :set -XTypeApplications
    :set -XScopedTypeVariables

    :set prompt "λ "

    import Prelude hiding (id, (.), FilePath)

    import Data.Function hiding (id, (.))
    import Data.Functor

    import Numeric.Natural
    import Data.Ratio

    import Data.Proxy
    import GHC.TypeLits

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

    import Control.Category
    import Control.Monad

    import System.Environment
    import System.Directory

    import Turtle hiding ((<&>), (%))
    import qualified Turtle.Bytes as Bytes

    :{
    ø ∷ Monoid m ⇒ m
    ø = mempty

    (‰) ∷ Semigroup a ⇒ a → a → a
    (‰) = (<>)

    (·) ∷ Functor f ⇒ (a → b) → f a → f b
    (·) = (<$>)

    (×) ∷ Functor f ⇒ f a → (a → b) → f b
    (×) = (<&>)

    (°) ∷ Applicative f ⇒ f (a → b) → f a → f b
    (°) = (<*>)

    (¿) ∷ Alternative f ⇒ f a → f a → f a
    (¿) = (<|>)

    unconsNE ∷ NE.NonEmpty a → (a, [a])
    unconsNE (x NE.:| xs) = (x, xs)

    ßprocGeneric cmd f applyF
      = T.words cmd
      & NE.nonEmpty
      & maybe (fail "Empty command") (unconsNE >>> uncurry f >>> applyF)

    ßproc ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m ExitCode
    ßproc cmd input = ßprocGeneric cmd proc ($ input)
    øßproc = flip ßproc ø

    ßprocs ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m ()
    ßprocs cmd input = ßprocGeneric cmd procs ($ input)
    øßprocs = flip ßprocs ø

    ßinproc ∷ Text → Shell Line → Shell Line
    ßinproc cmd input = ßprocGeneric cmd inproc ($ input)
    øßinproc = flip ßinproc ø

    ßinprocWithErr ∷ Text → Shell Line → Shell (Either Line Line)
    ßinprocWithErr cmd input = ßprocGeneric cmd inprocWithErr ($ input)
    øßinprocWithErr = flip ßinprocWithErr ø

    ßprocStrict ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m (ExitCode, Text)
    ßprocStrict cmd input = ßprocGeneric cmd procStrict ($ input)
    øßprocStrict = flip ßprocStrict ø

    ßprocStrictWithErr ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m (ExitCode, Text, Text)
    ßprocStrictWithErr cmd input = ßprocGeneric cmd procStrictWithErr ($ input)
    øßprocStrictWithErr = flip ßprocStrictWithErr ø

    µßproc ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m ExitCode
    µßproc cmd input = ßprocGeneric cmd Bytes.proc ($ input)
    øµßproc = flip µßproc ø

    µßprocs ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m ()
    µßprocs cmd input = ßprocGeneric cmd Bytes.procs ($ input)
    øµßprocs = flip µßprocs ø

    µßinproc ∷ Text → Shell BS.ByteString → Shell BS.ByteString
    µßinproc cmd input = ßprocGeneric cmd Bytes.inproc ($ input)
    øµßinproc = flip µßinproc ø

    µßinprocWithErr ∷ Text → Shell BS.ByteString → Shell (Either BS.ByteString BS.ByteString)
    µßinprocWithErr cmd input = ßprocGeneric cmd Bytes.inprocWithErr ($ input)
    øµßinprocWithErr = flip µßinprocWithErr ø

    µßprocStrict ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m (ExitCode, BS.ByteString)
    µßprocStrict cmd input = ßprocGeneric cmd Bytes.procStrict ($ input)
    øµßprocStrict = flip µßprocStrict ø

    µßprocStrictWithErr ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m (ExitCode, BS.ByteString, BS.ByteString)
    µßprocStrictWithErr cmd input = ßprocGeneric cmd Bytes.procStrictWithErr ($ input)
    øµßprocStrictWithErr = flip µßprocStrictWithErr ø

    shellList :: MonadIO m ⇒ Shell a → m [a]
    shellList = reduce (Fold (flip (:)) [] id)
    :}

    -- Aliases
    :{
    l args = procs "ls" (["--color=auto", "-lah"] ‰ args) ø
    l' = l ø
    ll args = procs "ls" (["--color=auto", "-lAh"] ‰ args) ø
    ll' = ll ø

    v = procs "nvim" `flip` ø
    v' = v ø
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
      ]} "$file"
    '';
  };
in

hell
