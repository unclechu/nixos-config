# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

type
  State* = enum on, off, toggle

  MaybeKind* = enum Just, Nothing
  Maybe*[T]  = object
    case kind*: MaybeKind
      of Just: value*: T
      of Nothing: discard

  EitherKind*   = enum Left, Right
  Either*[L, R] = object
    case kind*: EitherKind
      of Left:  left*:  L
      of Right: right*: R

proc just*[T](x: T): Maybe[T] {.inline.} = Maybe[T](kind: Just, value: x)
proc nothing*[T](): Maybe[T]  {.inline.} = Maybe[T](kind: Nothing)
proc isJust*[T](x: Maybe[T]):    bool {.inline.} = x.kind == Just
proc isNothing*[T](x: Maybe[T]): bool {.inline.} = x.kind == Nothing

proc left*[L, R](x: L): Either[L, R] {.inline.} =
  Either[L, R](kind: Left, left: x)
proc right*[L, R](x: R): Either[L, R] {.inline.} =
  Either[L, R](kind: Right, right: x)
proc isLeft*[L, R](x: Either[L, R]):  bool {.inline.} = x.kind == Left
proc isRigth*[L, R](x: Either[L, R]): bool {.inline.} = x.kind == Right
