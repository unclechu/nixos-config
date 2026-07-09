# Not needed, just keeping nimlsp happy
when defined(nimsuggest) and not defined(nimscript): import system/nimscript

switch("mm", "atomicArc")

if getCommand() == "check":
  switch("styleCheck", "error")
  switch("hintAsError", "XDeclaredButNotUsed:on")
