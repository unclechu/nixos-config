# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Either type implementation.
#
# See https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Prelude.html#t:Either

type
  Either*[L, R] = object
    case isRight*: bool
    of false: left*: L
    of true: right*: R

template left*[L, R](x: L): Either[L, R] = Either[L, R](isRight: false, left: x)
template right*[L, R](x: R): Either[L, R] = Either[L, R](isRight: true, right: x)

template map*[L, Ra, Rb](x: Either[L, Ra], f: proc (val: Ra): Rb): Either[L, Rb] =
  case x.isRight
  of true: right[L, Rb](f(x.right))
  of false: left[L, Rb](x.left)

template mapLeft*[La, R, Lb](x: Either[La, R], f: proc (val: La): Lb): Either[Lb, R] =
  case x.isRight
  of false: left[Lb, R](f(x.left))
  of true: right[Lb, R](x.right)

# `flatMap` because `bind` is a reserved Nim builtin
template flatMap*[L, Ra, Rb](x: Either[L, Ra], f: proc (val: Ra): Either[L, Rb]): Either[L, Rb] =
  case x.isRight
  of true: f(x.right)
  of false: left[L, Rb](x.left)
