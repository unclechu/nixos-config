# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# High-level interface for parsing command-line arguments.
#
# Built on top of `std/parseopt`.
#
# It also adds raw arguments separator support `--` which `std/parseopt`
# doesn’t support out of the box.

from std/options import nil
from std/os import nil
from std/parseopt import nil
from std/sets import nil
from std/strutils import nil
from std/tables import `[]=`, `[]`

const strOptionDefaultPlaceholder: string = "TEXT"

type
  ArgOptionValueStateKind* = enum flagOption strOption

  ArgOptionValueState* {.requiresInit.} = object
    case kind*: ArgOptionValueStateKind
    of flagOption: flagValue*: bool = false
    of strOption:
      strValue*: options.Option[string] = options.none(string)

      # Accumulator for multiple string option occurrences.
      #
      # While `strValue` is just overridden by the latest occurrence of the option
      # (which is common behavior for a single option value) `strValueAcc` collects
      # all values sequentially, preserving the order. So this allows to treat the
      # option as a list of values provided.
      #
      # In short:
      #
      # - For a single value option use `strValue`
      # - For multiple values option with the same option name use `strValuesAcc`
      strValuesAcc*: seq[string] = @[]

      # A value placeholder rendered for the option in the usage info
      strPlaceholder*: string = strOptionDefaultPlaceholder

  ArgOptionValueStateRef* = ref ArgOptionValueState

  ArgOption* {.requiresInit.} = object
    short*: options.Option[char] = options.none(char)
    isRequired*: bool = false
    info*: string = ""
    # Note that `ArgOptionValueState∷kind` distinguishes between a string option or a flag
    value*: ArgOptionValueStateRef

  ArgOptionRef* = ref ArgOption

  ArgOptions* = tables.OrderedTable[string, ArgOptionRef]

  ParsedArgs* {.requiresInit.} = object
    opts*: ArgOptions
    positional*: seq[string]

# Construct a flag option
template makeFlagOption*(
  optShort: char = '\0',
  optRequired: bool = false,
  optInfo: string = "",
): ArgOptionRef =
  ArgOptionRef(
    short: if optShort != '\0': options.some(optShort) else: options.none(char),
    isRequired: optRequired,
    info: optInfo,
    value: ArgOptionValueStateRef(kind: flagOption),
  )

# Construct a string option
template makeStrOption*(
  optShort: char = '\0',
  optRequired: bool = false,
  optInfo: string = "",
  optPlaceholder: string = strOptionDefaultPlaceholder,
): ArgOptionRef =
  ArgOptionRef(
    short: if optShort != '\0': options.some(optShort) else: options.none(char),
    isRequired: optRequired,
    info: optInfo,
    value: ArgOptionValueStateRef(kind: strOption, strPlaceholder: optPlaceholder),
  )

# Construct an `optsSpec` value for the `parseArgs` function.
template makeOptionsSpec*(pairs: openArray[(string, ArgOptionRef)]): ArgOptions =
  tables.toOrderedTable(pairs)

# Read an option value by its name from the parsed arguments.
template getOptionValue*(args: ParsedArgs, optName: string): ArgOptionValueStateRef =
  args.opts[optName].value

# A helper to deep-clone `cloneArgOptionValueState` value.
template cloneArgOptionValueState(x: ArgOptionValueStateRef): ArgOptionValueStateRef =
  case x.kind
  of flagOption: ArgOptionValueStateRef(kind: flagOption, flagValue: x.flagValue)
  of strOption:
    ArgOptionValueStateRef(
      kind: strOption,
      strValue: x.strValue,
      strValuesAcc: x.strValuesAcc,
      strPlaceholder: x.strPlaceholder,
    )

# A helper to deep-clone `ArgOptionRef` value.
template cloneArgOption(x: ArgOptionRef): ArgOptionRef =
  ArgOptionRef(
    short: x.short, # nimlsp bugs out here, ignore
    isRequired: x.isRequired,
    info: x.info,
    value: cloneArgOptionValueState(x.value),
  )

# A helpers to apply automatic word-splitting line-breaking respecting the maximum line length.
template limitLineLenWithWordSplit(inputLines: seq[string], maxLineLength: int): seq[string] =
  block:
    var newLinesAcc: seq[string] = @[]
    for line in inputLines:
      # Respect empty new lines from the input
      if line.len <= 0: (newLinesAcc.add(""); continue)
      var acc: string = ""
      for word in strutils.split(line, " "):
        if acc.len <= 0: acc = word # If single word is exceeding the limit adding it anyway.
        elif (acc.len + 1 + word.len) > maxLineLength: (newLinesAcc.add(acc); acc = word)
        else: acc &= " " & word
      if acc.len > 0: newLinesAcc.add(acc)
    newLinesAcc

# Parse the arguments using the high-level `optsSpec` specification for the arguments.
#
# Updated `optsSpec` is returned from the function with added parsed state for
# the arguments. But `optsSpec` is cloned, so one from the input is not modified.
# `ArgOptions` is used both for providing specification for the arguments
# and for providing parsed state for them.
#
# For building the specification use higher-level helpers:
#
#   - `makeOptionsSpec` for making a value for `optsSpec`
#   - `makeStrOption` for declaring a string option
#   - `makeFlagOption` for declaring a flag option
#
# For reading the parsed options use:
#
#   - `getOptionValue`
#
# Usage example:
#
#   const
#     fooOpt: string = "foo"
#     verboseOpt: string = "verbose"
#
#   let args: cliargs.ParsedArgs = cliargs.parseArgs(
#     optsSpec = cliargs.makeOptionsSpec({
#       fooOpt: cliargs.makeStrOption(optInfo = "Example string option"),
#       verboseOpt: cliargs.makeFlagOption(optShort = 'v', optInfo = "Enable verbose logging."),
#       "help": cliargs.makeFlagOption(optShort = 'h', optInfo = "Show this usage information."),
#     }),
#     usageDescription = "Test application"
#   )
#
#   let
#     foo: options.Option[string] = cliargs.getOptionValue(args, fooOpt).strValue
#     verbose: bool = cliargs.getOptionValue(args, verboseOpt).flagValue
#
#   echo "--foo: " & $foo
#   echo "--verbose: " & $verbose
#   echo "positional args: " & $args.positional
#
# The call the app with arguments: `--foo hi -v foo -- bar`. Then printed result is:
#
#   --foo: (val: "hi", has: true)
#   --verbose: true
#   positional args: @["foo", "bar"]
#
# For `-h` you get:
#
#   Test application
#
#   Usage: pointer-setup [OPTIONAL]
#
#   OPTIONAL options:
#     --foo=TEXT    Example string option
#     -v --verbose  Enable verbose logging.
#     -h --help     Show this usage information.
#
proc parseArgs*(
  optsSpec: ArgOptions,
  # Flag option that requests usage info (`--help` by default).
  # Note that if you don’t add `help` (or whatever you specify in `usageRequestFlag`) to `optsSpec`
  # there will be no flag that requests the usage info. It will be only rendered in case of parsing
  # or validation error.
  usageRequestFlag: string = "help",
  usageDescription: string = "", # You application description to render before the usage info
  usageLineLenLimit: int = 79, # Word-based line splitting applied for `info` when line overflows
  usageIndent: string = "  ", # For nested sections
  usageColumnSpacer: string = "  ", # Spacer between args and info for them
  usageRequiredPlaceholder: string = "REQUIRED",
  usageRequiredSectionLabel: string = "REQUIRED options:",
  usageOptionalPlaceholder: string = "[OPTIONAL]",
  usageOptionalSectionLabel: string = "OPTIONAL options:",
  usagePositionalPlaceholder: string = "", # Not showing the placeholder by default
  usagePositionalSectionLabel: string = "Positional arguments:",
  usagePositionalSectionInfo: string = "", # Not showing positional info section by default
  usageAddLineSpacer: bool = true, # Extra lines around multiline option `info` (better separation)
  usageLineSpacerRendersColumn: bool = false, # Render `usageColumnSpacer` for `usageAddLineSpacer`
  # An optional function that will transform generate usage info string.
  # You can for instance add extra empty lines in front and after like this:
  # ```
  # usageInfoTransform = proc (usage: string, errors: string): string =
  #   (if errors.len > 0: "\n" & errors & "\n" else: "") & "\n" & usage & "\n"
  # ```
  # Note that if there are no errors, just usage info rendering then `errors` is `""`
  usageInfoTransform: (proc (usageInfo: string, errors: string): string) = nil,
  outWriteLine: (proc (line: string): void) = (proc (line: string): void = stdout.writeLine(line)),
  errWriteLine: (proc (line: string): void) = (proc (line: string): void = stderr.writeLine(line)),
  # By default the raw arguments are read from application argv.
  # But you can customize it by providing a `getRawArgs` callback.
  # You can pre-process some of the arguments for example in case your use case
  # scenario requires it.
  getRawArgs: (proc (): seq[string]) = (proc (): seq[string] = os.commandLineParams()),
  # Returning a non-empty string means it’s a validation failure message printed with `errWriteLine`
  # and exiting the app with exit code 2.
  # If you want to do things differently in case of validation failiure
  # you can just `quit` inside this callback instead of returning validation error string.
  # And you can use the `usageInfo` string if you also need to render usage information.
  validate: (proc (args: ParsedArgs, usageInfo: string): string) = nil,
  # Rendered only if you set `validate`
  validationErrorLabel: string = "Arguments validation failed:",
): ParsedArgs =
  var
    # Postitional arguments accumulator
    positional: seq[string] = @[]

    # Cloned `ArgOptions` spec.
    # `ArgOptions` is used both as a spec for the arguments
    # and as an options parsed state value.
    opts: ArgOptions = block:
      var cloned: ArgOptions = tables.initOrderedTable[string, ArgOptionRef]()
      for k, v in tables.pairs(optsSpec): cloned[k] = cloneArgOption(v)
      cloned

  # Generate usage info
  proc genUsageInfo(): string =
    var hasRequired: bool = false
    var hasOptional: bool = false

    # Figure out values for `hasRequired` and `hasOptional` flags.
    # This flags are needed to determine if rendering required and/or optional
    # options section is needed.
    for v in tables.values(opts):
      if v.isRequired: hasRequired = true else: hasOptional = true

    # Accumulator of all lines for the usage information
    var usageLines: seq[string] = @[]

    # Add application description in front if it is provided in the arguments
    if usageDescription.len > 0:
      let descLines: seq[string] = limitLineLenWithWordSplit(
        strutils.split(usageDescription, "\n"),
        usageLineLenLimit
      )
      if descLines.len > 0: (usageLines.add(descLines); usageLines.add(""))

    # Starting of the usage section
    usageLines.add(
      "Usage: " & os.extractFilename(os.getAppFilename()) &
        (if hasRequired and (usageRequiredPlaceholder != ""):
          " " & usageRequiredPlaceholder else: "") &
        (if hasOptional and (usageOptionalPlaceholder != ""):
          " " & usageOptionalPlaceholder else: "") &
        (if usagePositionalPlaceholder != "":
          " " & usagePositionalPlaceholder else: ""),
    )

    # Options to render on the left side/column.
    # For example key-value item would be `foo` ⇒ `  -f --foo=TEXT`.
    var optsLeft: tables.Table[string, string] = tables.initTable[string, string]()
    for k, v in tables.pairs(opts):
      var line: string = usageIndent
      options.map(v.short, proc (x: char): void = line &= "-" & x & " ")
      line &= "--" & k
      case v.value.kind
      of flagOption: discard
      of strOption: line &= "=" & v.value.strPlaceholder
      optsLeft[k] = line

    # Amount of chars before option description starts.
    # For example if your option renders as `  -f --foo=TEXT  Some information`
    # then the amount of chars here is for `  -f --foo=TEXT` part.
    # This helps to column-align all of the options.
    # The value is calculated for the longest option of all.
    let leftPad: int = block:
      var acc: int = 0
      for v in tables.values(optsLeft): (if v.len > acc: acc = v.len)
      acc

    # The space that is left for the options information column after subtracting
    # the width of the options left column from the maximum line length.
    let infoMax: int = usageLineLenLimit - (leftPad + usageColumnSpacer.len)
    if infoMax <= 0: raiseAssert "No space left for info (consider increasing usageLineLenLimit)"

    # Split optional and required options.
    # The required and optional options sections are rendered separately.
    # In the provided spec required and optional options can be mixed,
    # but here they are separated.
    var
      requiredOpts: ArgOptions = tables.initOrderedTable[string, ArgOptionRef]()
      optionalOpts: ArgOptions = tables.initOrderedTable[string, ArgOptionRef]()
    for k, v in tables.pairs(opts):
      if v.isRequired: requiredOpts[k] = v else: optionalOpts[k] = v

    # Render options arguments section (either required options section or optional options)
    template renderOptionsSection(isShowingRequiredArgs: bool): void =
      block:
        let
          opts: ArgOptions = if isShowingRequiredArgs: requiredOpts else: optionalOpts
          noInfoColumn: bool =
            # Only render column when `usageLineSpacerRendersColumn` is enabled
            # and there is at least one option with `info` in the options section.
            usageLineSpacerRendersColumn and (block:
              var x: bool = false
              for opt in tables.values(opts): (if opt.info.len > 0: (x = true; break))
              x
            )
          lineSpacer: string =
            # Do not render column for the spacer if nothing else renders it in the options section.
            if (not usageLineSpacerRendersColumn) or (not noInfoColumn): "" else:
            strutils.repeat(' ', leftPad) & usageColumnSpacer

        var i: int = 0
        for k, v in tables.pairs(opts):
          var line: string = optsLeft[k]
          line &= strutils.repeat(' ', leftPad - line.len)

          let newInfoLines: seq[string] =
            limitLineLenWithWordSplit(strutils.split(v.info, "\n"), infoMax)

          # Add an extra line spacer before for multi-line info
          # but only if it’s not the first option in the section
          # and there is no spacer already added before it.
          if (
            i > 0 and
            newInfoLines.len > 1 and
            usageLines.len > 0 and
            newInfoLines.len > 1 and
            usageLines[usageLines.len - 1] != lineSpacer
          ):
            usageLines.add(lineSpacer)

          # Add the information column content for the option
          for n, infoLine in newInfoLines:
            if n == 0: usageLines.add(line & usageColumnSpacer & infoLine)
            else: usageLines.add(strutils.repeat(' ', leftPad) & usageColumnSpacer & infoLine)

          # No `info` for the option (rendering just the option)
          if newInfoLines.len <= 0:
            usageLines.add(line & (if noInfoColumn: usageColumnSpacer else: ""))

          # Add an extra line spacer after for multi-line info
          if i != (tables.len(opts) - 1) and newInfoLines.len > 1: usageLines.add(lineSpacer)

          inc i

    # Show required option arguments section if there are any required options
    if hasRequired:
      usageLines.add(["", usageRequiredSectionLabel])
      renderOptionsSection(true)

    # Show optional option arguments section if there are any optional options
    if hasOptional:
      usageLines.add(["", usageOptionalSectionLabel])
      renderOptionsSection(false)

    # Show information section about positional arguments if it’s provided in the arguments
    if usagePositionalSectionInfo != "":
      usageLines.add(["", usagePositionalSectionLabel])
      for line in limitLineLenWithWordSplit(
        strutils.split(usagePositionalSectionInfo, "\n"),
        usageLineLenLimit - usageIndent.len
      ):
        usageLines.add(usageIndent & line)

    strutils.join(usageLines, "\n")

  # Accumulator for parsing errors.
  # If at the end this list is not empty it means parsing failed.
  # The parsing doesn’t stop with first error, instead just adding to this list,
  # so that you can later show all of the parsing errors at once.
  var parsingErrors: seq[string] = @[]

  # If `value` is `none` it means expect a flag only (no string option)
  template parseShortOpt(opt: char, optVal: options.Option[string]): ArgOptionRef =
    let foundArg: ArgOptionRef = block:
      var found: ArgOptionRef = nil
      for v in tables.values(opts):
        if options.isSome(v.short) and options.get(v.short) == opt:
          (found = v; break)
      found
    if foundArg != nil:
      case foundArg.value.kind
      of flagOption:
        foundArg.value.flagValue = true
      of strOption:
        if options.isSome(optVal):
          foundArg.value.strValue = options.some(options.get(optVal))
          foundArg.value.strValuesAcc.add(options.get(optVal))
        else:
          raiseAssert("Unexpected short option without value: " & strutils.escape("-" & opt))
    foundArg

  # Parse shot option argument (e.g. `-h` or `-x=bar`)
  template parseShortOpts(key: string, value: string): void =
    let charKey: char = block:
      # Should not normally happen
      if key.len != 1:
        raiseAssert("Unexpected one-character short option, got: " & strutils.escape(key))
      key[0]
    if parseShortOpt(charKey, options.some(value)) == nil:
      # Note that in case of an unknown short option getopt can’t distinguish between
      # `-abc` being a non-flag `-a` + `bc` value or a set of short chars: `-a -b -c`.
      # So by default it treats it as a one short option + value.
      # And the error will be printed only for the `-a` in this case.
      # And in this case it will also not recognize valid short options like `-h` in case of
      # `-ah`. For that reason I treat all unrecognized short options as a set of short options.
      var shortOpts: sets.OrderedSet[char] = sets.initOrderedSet[char]()
      sets.incl(shortOpts, charKey)
      for c in value: sets.incl(shortOpts, c)
      for opt in sets.items(shortOpts):
        if parseShortOpt(opt, options.none(string)) == nil:
          parsingErrors.add("Unexpected short option: " & strutils.escape("-" & opt))

  var shortNoVal: set[char] = {}
  var longNoVal: seq[string] = @[]

  # Populate `shortNoVal` and `longNoVal`
  for k, v in tables.pairs(opts):
    case v.value.kind
    of strOption: discard
    of flagOption:
      longNoVal.add(k)
      options.map(v.short, proc (x: char): void = shortNoVal.incl(x))

  # Initialize `std/parseopt` arguments parser
  var parser: parseopt.OptParser = parseopt.initOptParser(
    cmdline = getRawArgs(),
    shortNoVal = shortNoVal,
    # `""` prevents `--` raw arguments separator from consuming the next argument as its value
    longNoVal = longNoVal & @[""],
  )

  # Collect and parse all arguments
  block parsingLoop:
    while true:
      parseopt.next(parser)
      case parser.kind
      of parseopt.cmdEnd: break parsingLoop
      of parseopt.cmdArgument: positional.add(parser.key)
      of parseopt.cmdShortOption: parseShortOpts(parser.key, parser.val)
      of parseopt.cmdLongOption:
        if parser.key == "": # `--` raw arguments separator
          # Everything after `--` is raw positional arguments
          positional.add(parseopt.remainingArgs(parser))
          break parsingLoop
        elif not tables.hasKey(opts, parser.key):
          parsingErrors.add("Unexpected long option: " & strutils.escape("--" & parser.key))
        else:
          case opts[parser.key].value.kind
          of strOption:
            opts[parser.key].value.strValue = options.some(parser.val)
            opts[parser.key].value.strValuesAcc.add(parser.val)
          of flagOption:
            opts[parser.key].value.flagValue = true

  # Render requested usage info and exit the app with success
  block renderRequestedUsageInfo:
    if not tables.hasKey(opts, usageRequestFlag): break renderRequestedUsageInfo
    var helpVal: ArgOptionValueStateRef = opts[usageRequestFlag].value
    case helpVal.kind
    of strOption: discard
    of flagOption:
      if helpVal.flagValue:
        let usage: string = genUsageInfo()
        let x: string = if usageInfoTransform != nil: usageInfoTransform(usage, "") else: usage
        outWriteLine(x)
        quit 0

  # Reporting missing required option validation error
  template reportMissingRequired(k: string, v: ArgOptionRef): void =
    parsingErrors.add(
      "Missing required argument: " &
      (if options.isSome(v.short): strutils.escape("-" & options.get(v.short)) & " / " else: "") &
      strutils.escape("--" & k)
    )

  # Validate required options
  for k, v in tables.pairs(opts):
    if v.isRequired:
      let value: ArgOptionValueStateRef = v.value
      case value.kind
      of flagOption: (if not value.flagValue: reportMissingRequired(k, v))
      of strOption: (if options.isNone(value.strValue): reportMissingRequired(k, v))

  # If there are parsing errors then print the parsing errors and exit the app with failure
  if parsingErrors.len > 0:
    let usage: string = genUsageInfo()
    let errors: string = strutils.join(parsingErrors, "\n")
    if usageInfoTransform != nil:
      errWriteLine(usageInfoTransform(usage, errors))
    else:
      errWriteLine(errors & "\n")
      errWriteLine(usage)
    quit 2

  # Final parsed result
  let args: ParsedArgs = ParsedArgs(opts: opts, positional: positional)

  # If validation callback is provided then perform that extra validation
  if validate != nil:
    let usage: string = genUsageInfo()
    var errors: seq[string] = @[]

    let validationError: string = validate(args, usage)
    if validationError.len > 0:
      errors.add(validationErrorLabel)

      for line in limitLineLenWithWordSplit(
        strutils.split(validationError, "\n"),
        usageLineLenLimit - usageIndent.len
      ):
        errors.add(usageIndent & line)

      if usageInfoTransform != nil:
        errWriteLine(usageInfoTransform(usage, strutils.join(errors, "\n")))
      else:
        for line in errors: errWriteLine(line)
        errWriteLine("\n" & usage)

      quit 2

  args
