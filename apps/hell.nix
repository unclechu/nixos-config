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

  ghciScript = writeText "hell-ghci-script" ''
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
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Char8 as BSC
    import qualified Data.ByteString.Lazy as BSL
    import qualified Data.ByteString.Lazy.Char8 as BSLC

    import qualified Data.List.NonEmpty as NE

    import Control.Category
    import Control.Monad

    import System.Environment
    import System.FilePath
    import System.Directory

    import Turtle hiding ((<&>))
    import qualified Turtle.Bytes as TB

    :{
    ø ∷ Monoid m ⇒ m
    ø = mempty

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

    ßprocs ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m ()
    ßprocs cmd input = ßprocGeneric cmd procs ($ input)

    ßinproc ∷ Text → Shell Line → Shell Line
    ßinproc cmd input = ßprocGeneric cmd inproc ($ input)

    ßinprocWithErr ∷ Text → Shell Line → Shell (Either Line Line)
    ßinprocWithErr cmd input = ßprocGeneric cmd inprocWithErr ($ input)

    ßprocStrict ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m (ExitCode, Text)
    ßprocStrict cmd input = ßprocGeneric cmd procStrict ($ input)

    ßprocStrictWithErr ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell Line → m (ExitCode, Text, Text)
    ßprocStrictWithErr cmd input = ßprocGeneric cmd procStrictWithErr ($ input)

    µßproc ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m ExitCode
    µßproc cmd input = ßprocGeneric cmd TB.proc ($ input)

    µßprocs ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m ()
    µßprocs cmd input = ßprocGeneric cmd TB.procs ($ input)

    µßinproc ∷ Text → Shell BS.ByteString → Shell BS.ByteString
    µßinproc cmd input = ßprocGeneric cmd TB.inproc ($ input)

    µßinprocWithErr ∷ Text → Shell BS.ByteString → Shell (Either BS.ByteString BS.ByteString)
    µßinprocWithErr cmd input = ßprocGeneric cmd TB.inprocWithErr ($ input)

    µßprocStrict ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m (ExitCode, BS.ByteString)
    µßprocStrict cmd input = ßprocGeneric cmd TB.procStrict ($ input)

    µßprocStrictWithErr ∷ (MonadIO m, MonadFail m) ⇒ Text → Shell BS.ByteString → m (ExitCode, BS.ByteString, BS.ByteString)
    µßprocStrictWithErr cmd input = ßprocGeneric cmd TB.procStrictWithErr ($ input)

    shellList :: MonadIO m ⇒ Shell a → m [a]
    shellList = reduce (Fold (flip (:)) [] id)
    :}

    -- Aliases
    :{
    vi = procs "nvim" `flip` ø
    v = vi []
    :}
  '';

  coloredPrompt = color:
    assert builtins.elem color ["red" "green"];
    let
      colorize = c: s:
        assert builtins.isInt c;
        assert builtins.isString s;
        ''\ESC[${toString c}m\STX${s}\ESC[m\STX'';
    in
    writeText "colored-hell-ghci-script-prompt-${color}" ''
      :set prompt "${colorize (if color == "red" then 31 else 32) "λ"} "
    '';

  hell = writeShellApplication {
    name = "hell";
    runtimeInputs = [ ghc ];

    text = ''
      ${lib.escapeShellArgs [
        "ghci"
        "-ignore-dot-ghci"
        "-ghci-script"
      ]} <(${
        builtins.concatStringsSep " ; " [
          (lib.escapeShellArgs [ "cat" "--" "${ghciScript}" ])
          "if (( UID == 0 ))"
          "then cat -- ${esc (coloredPrompt "red")}"
          "else cat -- ${esc (coloredPrompt "green")}"
          "fi"
        ]
      })
    '';
  };
in

hell
