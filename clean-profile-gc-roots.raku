#! /usr/bin/env raku

# A script that cleans up Nix profile generations (in a specific way) to free some space.
#
# It shows you a cleanup plan first and then asks you to confirm the destructive operation.
# It keeps one generation of each profile which is just current generation of that profile.
# Also in case of a system profile it keeps latest generation of each NixOS release version.
#
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

use v6.d;
use fatal; # Prevent masking failures by “fail” call
use nqp; # Solely for nqp::readlink

# Change working directory to the directory of the script
$*PROGRAM.dirname.&*chdir;

# The main directory of Nix profiles where to find profile symlinks recursively in
my IO::Path:D \nixProfilesRootDir = '/nix/var/nix/profiles'.IO;

# Root directory of current user profiles to find profile symlinks recursively in.
#
# For optional “--nuke-user-home-profiles” feature.
my IO::Path:D \userHomeNixProfilesRootDir =
  %*ENV<HOME>.IO.add(".local/state/nix/profiles");

subset NonEmptyStr of Str where .trim !~~ '';
subset NonEmptyArr of Array where .elems > 0;

# Explicit Nil-like value for optional values.
#
# When you assign Nil to a container it changes to Any that does not match with explicit Nil.
# This is kind of like universal Nil that is not prone to this issue.
subset None where !.defined;

package Util {
  # Get symlink’s target (shallowly, without following symlinks).
  #
  # Make sure “:CWD” is correct. It should be the same directory as where the “filePath” is placed.
  # Symlink target is relative to the symlink’s location (if the target path is relative/not
  # absolute). “:CWD” defaults to the directory of “filePath” so in theory the default value is the
  # correct one.
  our sub readlink(IO::Path:D \filePath, IO::Path :$CWD --> IO::Path:D) {
    IO::Path.new(nqp::readlink(filePath.relative), :CWD($CWD // filePath.dirname))
  }

  # Some logging helpers
  package Log {
    # Shell text coloring helpers
    our sub red(Str:D \x --> NonEmptyStr:D) { "\e[31m{x}\e[0m" }
    our sub green(Str:D \x --> NonEmptyStr:D) { "\e[32m{x}\e[0m" }
    our sub blue(Str:D \x --> NonEmptyStr:D) { "\e[34m{x}\e[0m" }
    our sub yellow(Str:D \x --> NonEmptyStr:D) { "\e[1;33m{x}\e[0m" }

    # More abstract shell text coloring helpers
    our sub info(Str:D \x --> NonEmptyStr:D) { blue x }
    our sub success(Str:D \x --> NonEmptyStr:D) { green x }
    our sub danger(Str:D \x --> NonEmptyStr:D) { red x }
    our sub warning(Str:D \x --> NonEmptyStr:D) { yellow x }
  }

  # Pretty-print kind of serialization
  package Show {
    # A regular hash
    our subset ShowableHash of Hash where .values.all ~~ ::('Showable');

    # A list of pairs
    our subset ShowableHashIsh where
      .elems > 0 # Constraining to non-zero elements to distinguish it from a simple list
      && .all ~~ Pair:D
      && .map(*.key).all ~~ ::('Showable')
      && .map(*.value).all ~~ ::('Showable')
      ;

    our subset ShowableArray of Array where .all ~~ ::('Showable');
    our subset ShowableList of List where .all ~~ ::('Showable');

    our subset ShowCallable of Callable where .returns ~~ NonEmptyStr;
    our subset ShowableSmth where .^find_method('show') ~~ ShowCallable:D;

    # A sum type of supported types for being “show”ed
    our subset Showable where
      None | Int:D | Str:D | IO::Path:D |
      ShowableHash:D | ShowableHashIsh:D | ShowableArray:D | ShowableList:D |
      ShowableSmth:D # Anything else that implements “show(--> NonEmptyStr:D)” method
      ;

    # Basic types
    our sub nil(None \x --> NonEmptyStr:D) { 'Nil' }
    our sub int(Int:D \x --> NonEmptyStr:D) { x.raku }
    our sub str(Str:D \x --> NonEmptyStr:D) { x.raku }
    our sub path(IO::Path:D \x --> NonEmptyStr:D) { x.absolute.raku }

    # List printer
    our sub list(ShowableList:D \x --> NonEmptyStr:D) {
      '(' ~ x.map({ show $_ }).join(', ') ~ ')'
    }

    # Array printer
    our sub array(ShowableArray:D \x --> NonEmptyStr:D) {
      my Str:D @lines;
      for @(x) -> Showable \item {
        @lines[@lines.elems-1] ~= ',' if @lines.elems > 0; # Add commas between entries
        @lines.append: show(item).split("\n")
      };
      $_ = @lines.map("\t"~*).Array.unshift('[').push(']');
      .join(.elems > 2 ?? "\n" !! '') # Do not multiline empty array
    }

    # A key-value kind of list (a list of “Pair”s)
    our sub pairs(Pair:D @kv --> NonEmptyStr:D) {
      return '{}' if @kv.elems < 1;
      my Str:D @lines;
      for @kv.sort(*.key) -> Pair:D \pair {
        my Showable \k = pair.key;
        my Showable \v = pair.value;
        my NonEmptyArr:D \showLines = show(v).split("\n").Array;
        @lines[@lines.elems-1] ~= ',' if @lines.elems > 0; # Add commas between entries
        showLines[0] = "{show(k)} => {showLines[0]}";
        @lines.append: showLines
      }
      @lines.map("\t"~*).Array.unshift('{').push('}').join("\n")
    }

    # Hash printer
    our sub hash(ShowableHash:D \x --> NonEmptyStr:D) {
      my Pair:D @pairs = x.pairs.Array;
      pairs @pairs
    }
    # Hash-ish (a list of pairs) printer
    our sub hashIsh(ShowableHashIsh:D \x --> NonEmptyStr:D) {
      my Pair:D @pairs = x.Array;
      pairs @pairs
    }

    # Generic printer for all supported types
    our sub show(Showable \x --> NonEmptyStr:D) {
      my Str:D @lines;
      given x {
        when None { @lines.push: Show::nil $_ }
        when Int:D { @lines.push: Show::int $_ }
        when Str:D { @lines.push: Show::str $_ }
        when IO::Path:D { @lines.push: Show::path $_ }
        when ShowableHash:D { @lines.append: Show::hash($_).split("\n").Array }
        when ShowableHashIsh:D { @lines.append: Show::hashIsh($_).split("\n").Array }
        when ShowableArray:D { @lines.append: Show::array($_).split("\n").Array }
        when ShowableList:D { @lines.append: Show::list($_).split("\n").Array }
        when ShowableSmth:D { @lines.append: .show.split("\n").Array }
        default { fail "Unexpected value of type {.WHAT.raku}: {.raku}" }
      }
      @lines.join: "\n"
    }
  }
}

# Nix profile symlinks-related types and functions.
package ProfileSymlink {
  package Types {
    # A profile symlink file.
    subset SymlinkFile where (
      # File path to a profile symlink file (“/nix/var/…/system…”)
      :profileSymlink(IO::Path:D)
      # Resolve path where the profile symlink points to (“/nix/store/…”)
      :pointsTo(IO::Path:D)
    ).sort;

    # A plural form of Nix profile symlinks.
    subset SymlinkFiles where .all ~~ SymlinkFile:D;

    # Currently selected profile generation.
    #
    # A symlink file with extra parsed data (profile name).
    subset CurrentProfile where (
      :profileName(NonEmptyStr:D) # For example “system”
      :profileSymlink(IO::Path:D) # File path to a profile symlink
      :pointsTo(IO::Path:D) # Profile symlink target path
    ).sort;

    subset NixosVersion of List where .elems == 2 && .all ~~ UInt:D;

    # 0 or more elements
    subset NixosVersionList where .all ~~ NixosVersion:D;

    # A profile generation.
    #
    # A symlink file with extra parsed data (profile name, generation number).
    subset Generation where (
      :profileName(NonEmptyStr:D) # For example “system”
      :generationNum(UInt:D) # For example “123”
      :profileSymlink(IO::Path:D) # File path to a profile symlink
      :pointsTo(IO::Path:D) # Profile symlink target path
      :systemProfileVersion(None | NixosVersion:D) # Nil for non-system profiles
      # Full NixOS version (including patch and commit hash suffix, e.g. “23.11.2774.3dc440faeee9”)
      :systemProfilePreciseVersion(None | NonEmptyStr:D)
    ).sort;

    # Plural form of “Generation”.
    #
    # Anything list-like all elements of which is “Generation:D” type.
    subset Generations where .all ~~ Generation:D;

    # All data about single profile (about its symlinks).
    subset Profile where (
      :profile(CurrentProfile:D) # Current profile symlink
      :generations(Generations:D) # All profile generations symlink of this profile
    ).sort;

    # Plural key-value version of “Profile” type.
    subset ProfilesHash of Hash where .values.all ~~ ProfileSymlink::Types::Profile:D;

    # To-text conversion for some of the types.
    #
    # Pretty-printers basically. Useful for debugging.
    package Show {
      our sub profile(Profile:D \profile --> NonEmptyStr:D) { Util::Show::show profile }

      our sub profiles(Hash[Profile:D] \profiles --> NonEmptyStr:D) {
        my Str:D @lines;
        for profiles.Hash.pairs.sort(*.key) -> Pair:D \pair {
          my NonEmptyStr:D \profileName = pair.key;
          my ProfileSymlink::Types::Profile:D \profile = pair.value;
          @lines.push: "Profile {profileName.raku}: {Show::profile(profile)}";
        }
        "\n" ~ @lines.join("\n\n") ~ "\n"
      }

      our sub nixosVersion(Types::NixosVersion \nixosVersion --> NonEmptyStr:D) {
        "{nixosVersion[0]}.{nixosVersion[1].fmt: '%02d'}"
      }
    }
  }

  # Parsers for profile symlink files helping to get some data for making “ProfileSymlink::Types”.
  package Parse {
    # Current profile name (e.g. “system”)
    grammar ProfileName {
      regex TOP { <profileName> }
      regex profileName { <[ - _ a..z ]>+ }
    }

    # Profile generation symlink file name parser (e.g. “system-123-link”)
    grammar GenerationFileName {
      regex TOP { <profileName> '-' <generationNum> '-link' }
      regex generationNum { \d+ }
      method profileName { ProfileName.subparse(self.orig, :pos(self.to)) }
    }

    # NixOS version parser (e.g. “23.11.2217.d02d818f22c7”)
    grammar NixosVersion {
      token TOP { <first>'.'<second>'.'<third> ['.'<extra>]? }
      token first { \d**2 }
      token second { \d**2 }
      token third { \d+ }
      token extra { \S+ }
    }

    # Smaller variant of “NixosVersion” grammer with only two numbers (e.g. “23.05”).
    grammar NixosRelease {
      token TOP { \s* <major> '.' <minor> \s* }
      token major { \d ** 1..2 }
      token minor { \d ** 1..2 }
    }

    # Grammar “actions” for “NixosRelease" parser
    class NixosReleaseActions {
      # Convert it back to original list pair of the “Types::NixosVersion”
      method TOP ($/) { make ($<major>.UInt, $<minor>.UInt) }
    }

    our sub parseNixosRelease(NonEmptyStr:D \x --> Types::NixosVersion:D) {
      NixosRelease.parse(x, :actions(NixosReleaseActions.new)).made
    }

    # System profile extra info obtained from symlink target (like NixOS release version)
    grammar SystemProfileTarget {
      regex TOP { ( .* '/' )* ( .* '-' )* <nixosVersion> }
      method nixosVersion { NixosVersion.subparse(self.orig, :pos(self.to)) }
    }
  }

  # Obtain all the data about profile symlinks needed for this script.
  #
  # List all profiles, their generations, parse NixOS version for system profiles.
  # Will include all profile symlinks by recursively looking into subdirectories too.
  our sub getDirectoryProfiles(
    IO::Path:D \profilesDir,
    Str:D :$subDirPrefix = ''
    --> Types::ProfilesHash:D
  ) {
    my regex SystemProfileDetector { 'system' };

    # All profile symlinks in the specified directory
    my ProfileSymlink::Types::SymlinkFile:D @symlinks =
      profilesDir
      .dir(test => { profilesDir.add($_).IO.l }) # List all symlinks in the directory (profiles)
      .map({ profileSymlink => $_, pointsTo => Util::readlink($_) })
      .map(*.sort.List)
      .sort({ my IO::Path:D $ = $_.Hash<profileSymlink> })
      ;

    # Currently selected profile generations.
    #
    # For example “system” symlink file
    # (which points to one of the generations files like “system-123-link”).
    my ProfileSymlink::Types::CurrentProfile:D @currentProfiles = do {
      my ProfileSymlink::Types::CurrentProfile:D @acc;
      for @symlinks -> \symlink {
        $_ = ProfileSymlink::Parse::ProfileName.parse(symlink.Hash<profileSymlink>.basename);
        next unless .defined;
        @acc.push: (symlink.Hash, profileName => .<profileName>.Str).flat.sort.List
      }
      @acc
    };

    # All profile generation files.
    #
    # For example: “system-123-link” symlink file.
    my ProfileSymlink::Types::Generation:D @profileGenerations = do {
      my ProfileSymlink::Types::Generation:D @acc;
      for @symlinks -> \symlink {
        $_ = ProfileSymlink::Parse::GenerationFileName.parse(symlink.Hash<profileSymlink>.basename);
        next unless .defined;

        my Types::NixosVersion $systemProfileVersion = Nil;
        my Str $systemProfilePreciseVersion = Nil;

        # For a system profile also parse NixOS version from symlink target.
        # This is required for preserving at least one latest generation of each NixOS release.
        if $subDirPrefix.match(&SystemProfileDetector)
        || .<profileName>.match(&SystemProfileDetector) {
          temp $_ = ProfileSymlink::Parse::SystemProfileTarget.parse(symlink.Hash<pointsTo>);
          $systemProfileVersion = (.<nixosVersion><first>.UInt, .<nixosVersion><second>.UInt).list;
          $systemProfilePreciseVersion
            = .<nixosVersion><first>.Str ~ '.'
            ~ .<nixosVersion><second>.Str ~ '.'
            ~ .<nixosVersion><third>.Str
            ~ do { $_ = .<nixosVersion><extra>; .defined ?? '.' ~ .Str !! '' }
            ;
        }

        @acc.push: (
          symlink.Hash:p.grep({ .key ~~ 'profileSymlink' | 'pointsTo' }),
          profileName => .<profileName>.Str,
          generationNum => .<generationNum>.UInt,
          systemProfileVersion => $systemProfileVersion,
          systemProfilePreciseVersion => $systemProfilePreciseVersion,
        ).flat.sort.List
      }
      @acc
    };

    # A mapping between profile name and all the data about the profile.
    my ProfileSymlink::Types::Profile:D %profiles = do {
      my ProfileSymlink::Types::Profile:D %acc;
      for @currentProfiles -> ProfileSymlink::Types::CurrentProfile:D \profile {
        # Take generations for this profile only
        my ProfileSymlink::Types::Generation:D @generations =
          @profileGenerations.grep({ $_.Hash<profileName> eq profile.Hash<profileName> });

        # Add this profile to the hash of all profiles.
        #
        # Profile key in this hash is directory prefix (relative to the root profiles dir) plus the
        # profile name. Directory prefix is required in order to avoid names clashing (e.g.
        # different “per-user” directories each can have their own “channels” profile names).
        %acc{$subDirPrefix ~ profile.Hash<profileName>} = (
          profile => $(profile),
          generations => @generations,
        ).sort.List;
      }
      %acc
    };

    # Looking for other profiles in subdirectories too.
    do {
      my IO::Path:D @directories =
        profilesDir
        # List all directories that are not symlinks.
        # Profiles are symlinks to directories, so we need to exclude symlinks explicitly.
        .dir(test => { $_ ~~ $*SPEC.curupdir && profilesDir.add($_).IO ~~ :d & :!l })
        .sort
        ;

      for @directories -> IO::Path:D \dir {
        # Recursive profiles reading from a subdirectory
        my ProfileSymlink::Types::Profile:D %moreProfiles =
          getDirectoryProfiles(dir, :subDirPrefix($subDirPrefix ~ dir.basename ~ '/'));

        # Inject more profiles into root profiles hash
        for %moreProfiles.keys -> NonEmptyStr:D \k {
          fail "Unexpectedly clashing profile key detected: {k.raku}"
            ~ " ({%profiles{k}.Hash<profile>.Hash<profileSymlink>.Str.raku} vs."
            ~ " {%moreProfiles{k}.Hash<profile>.Hash<profileSymlink>.Str.raku})"
            if %profiles{k}:exists;

          %profiles{k} = %moreProfiles{k}
        }
      }
    }

    %profiles
  }
}

package Cleanup {
  package Types {
    # A derivative of “ProfileSymlink::Types::SymlinkFiles” with optional NixOS version.
    subset GenerationSymlink where (
      # File path to a profile symlink file (“/nix/var/…/system…”)
      :profileSymlink(IO::Path:D)
      # Resolve path where the profile symlink points to (“/nix/store/…”)
      :pointsTo(IO::Path:D)
      # Optional full NixOS version string (e.g. “23.11.2774.3dc440faeee9”)
      :systemProfilePreciseVersion(None | NonEmptyStr:D)
    ).sort;

    # A plural form of Nix generation profile symlink.
    subset GenerationSymlinks where .all ~~ GenerationSymlink:D;

    # A mapping between NixOS version (as a list pair of major and minor parts) and a list of
    # generation symlinks that are builds of the NixOS of that version.
    #
    # N.B. When passing the key make sure you wrap it into a scalar container (e.g.
    # “foo{$(23,11)}”). Otherwise “foo{(23,11)}” will be interpreted as “(foo{23},foo{11})”.
    # Depending on the context you may need a scalar wrapper for the value too.
    our class NixosVersionMap does Associative[
      # N.B. Both types are based on lists. Make sure you remember to add scalar wrappers like
      # “$(…)” to make sure it’s passed as a single value and not expanded into multiple.
      GenerationSymlinks:D, # Value
      ProfileSymlink::Types::NixosVersion:D, # Key
    ] {
      subset Key where NixosVersionMap.keyof; subset Keys where .all ~~ Key;
      subset Value where NixosVersionMap.of; subset Values where .all ~~ Value;
      subset ValueItem where GenerationSymlink:D;

      # The key is internally serialized into a string for the implementation simplicity. Hashes are
      # using “.WHICH” (by using “===”) for key equality checks, for simple values it works just
      # fine. But two separately created lists with identical values in them are not equal when you
      # apply “===” check (they are checked by their memory references, not by their values).
      has Value %!data{NonEmptyStr:D};

      # Standard methods for hash syntax.
      method AT-KEY(Key \k --> Value) { %!data{k.Str} }
      method EXISTS-KEY(Key \k --> Bool:D) { %!data{k.Str}:exists }
      method ASSIGN-KEY(Key \k, Value \v) { %!data{k.Str} = v }
      method elems(--> UInt:D) { %!data.elems }
      method keys(--> Keys:D) { %!data.keys.map(*.&Types::Parse::parseNixosVersionKey).List }
      method values(--> Values:D) { %!data.values.List }

      method kv(--> Seq:D) {
        %!data.kv.map({
          if $_ ~~ NonEmptyStr:D { Types::Parse::parseNixosVersionKey $_ } else { $_ }
        })
      }

      # Find a value (a list of symlinks) by key (or create a new one if it does not exist) and add
      # a new entry to the item to the value (add the specified symlink to the list).
      method add(Key \k, ValueItem \v) {
        if self{k}:exists { self{k}.push: $(v) } else { self{k} = [$(v)] }
      }

      # Find key by a value item.
      #
      # Value is a list of symlinks. Check each value trying to find the specified symlink in it.
      # If the match is found the key of the value is returned.
      method findKeyBySymlink(ValueItem \vItem --> Key) {
        for %!data.kv -> NonEmptyStr:D \k, Value \v {
          for v.List -> ValueItem \x {
            return (my Key $ = Types::Parse::parseNixosVersionKey k) if x eqv vItem
          }
        }
        Nil
      }

      # “Util::Show::Showable” implementation
      method show(--> NonEmptyStr:D) {
        my Pair:D @pairs = %!data.pairs.map({
          Pair.new(
            my Key $ = Types::Parse::parseNixosVersionKey(.key),
            my Value $ = .value,
          )
        }).Array;
        Util::Show::pairs @pairs
      }
    }

    # A plan for a single profile generation symlinks cleanup
    subset ProfileCleanupPlan where (
      :profileName(NonEmptyStr:D)
      :currentProfile(ProfileSymlink::Types::CurrentProfile:D)
      :generationsToKeep(GenerationSymlinks:D)
      :generationsToNuke(GenerationSymlinks:D)

      # Nil for non-system Nix profiles
      :systemNixosVersions(None | NixosVersionMap:D)
    ).sort;

    # A mapping between profile prefixed name (key) and cleanup plan for the profile (value).
    subset ProfilesCleanupPlanMap of Hash where
      .keys.all ~~ NonEmptyStr:D
      && .values.all ~~ ProfileCleanupPlan:D;

    package Parse {
      # A parser for serialized “ProfileSymlink::Types::NixosVersion:D” via “.Str” method
      grammar NixosVersionKey {
        token TOP { \s* <major> \s+ <minor> \s* }
        token major { \d ** 1..2 }
        token minor { \d ** 1..2 }
      }

      # Grammar “actions” for “NixosVersionKey" parser
      class NixosVersionKeyActions {
        # Convert it back to original list pair of the NixOS version
        method TOP ($/) { make ($<major>.UInt, $<minor>.UInt) }
      }

      our sub parseNixosVersionKey(NonEmptyStr:D \x --> ProfileSymlink::Types::NixosVersion:D) {
        NixosVersionKey.parse(x, :actions(NixosVersionKeyActions.new)).made
      }
    }
  }

  # Log messages generation for the cleanup plan
  package Log {
    my subset OptionalNixosVersionMap where None | Types::NixosVersionMap:D;
    my subset NixosVersionMarker of NonEmptyStr where 'full' | 'unprefixed';

    our sub renderPlan(Types::ProfilesCleanupPlanMap:D \planMap --> NonEmptyStr:D) {
      my Str:D @lines;
      sub printNoneIfEmpty(@x) { @x.elems > 0 ?? @x !! 'None' }

      for planMap.pairs.sort(*.key) -> Pair:D \pair {
        my (NonEmptyStr:D \k, Types::ProfileCleanupPlan:D \plan) = pair.key, pair.value;
        @lines.push: '' if @lines.elems > 0; # Empty line between profiles
        @lines.push: "Profile “{Util::Log::info k}”:";

        # Current profile generation symlink path
        my NonEmptyStr:D \currentGenPath = plan.Hash<currentProfile>.Hash<pointsTo>.absolute;

        my OptionalNixosVersionMap \nixosVersionMapping = plan.Hash<systemNixosVersions>;

        @lines.push: "\tCurrent profile: {Util::Log::info currentGenPath}";

        sub addMarkers(
          Types::GenerationSymlink:D \gen,
          NixosVersionMarker:D :$withNixosVersion = 'full'
          --> Str:D
        ) {
          my Bool:D \isCurrentGen = gen.Hash<profileSymlink>.absolute eq currentGenPath;

          my Str:D @markers = (
            isCurrentGen ?? 'current' !! Nil,
            !gen.Hash<systemProfilePreciseVersion>.defined ?? Nil !! (
              ($withNixosVersion eq 'unprefixed' ?? '' !! 'NixOS release ')
              ~ gen.Hash<systemProfilePreciseVersion>
            ),
          ).grep(*.defined);

          @markers.elems <= 0 ?? '' !! ' ' ~ Util::Log::info "({@markers.join: ', '})"
        }

        @lines.append: (
          "Generations will be {Util::Log::success 'kept'}:",

          (plan.Hash<generationsToKeep>
            .map({ Util::Log::success($_.Hash<profileSymlink>.absolute) ~ addMarkers($_) })
            ).&printNoneIfEmpty
            .map("\t"~*)
        ).flat.map("\t"~*);

        if nixosVersionMapping !~~ None {
          @lines.append: (
            'Keeping one latest generation of each NixOS version:',

            nixosVersionMapping
              .keys
              .sort # Order by NixOS version
              .reverse # Latest NixOS version is on the top
              .map({
                # A pair of NixOS release version and last preserved per-release generation
                Pair.new(
                  $_,
                  # Can be Nil if NixOS release was nuked (no preserved generations)
                  nixosVersionMapping{$($_)}.values.first({
                    plan.Hash<generationsToKeep>.first(* eqv $_)
                  })
                )
              })
              .grep(*.value !~~ Nil) # Filter out nuked NixOS releases
              .map({
                "NixOS {Util::Log::warning ProfileSymlink::Types::Show::nixosVersion(.key)}"
                  ~ ": {Util::Log::success .value.Hash<profileSymlink>.absolute}"
                  ~ addMarkers(.value, :withNixosVersion('unprefixed'))
              })
              .map("\t"~*)
          ).flat.map("\t"~*);
        }

        @lines.append: (
          "Generations will be {Util::Log::danger 'nuked'}:",

          (plan.Hash<generationsToNuke>
            .map({ Util::Log::danger($_.Hash<profileSymlink>.absolute) ~ addMarkers($_) })
            ).&printNoneIfEmpty
            .map("\t"~*)
        ).flat.map("\t"~*);
      }

      "\n" ~ @lines.join("\n") ~ "\n"
    }

    sub yesOrNo(--> Bool:D) {
      print
        Util::Log::warning('Type either ')
        ~ Util::Log::info('(y)es') ~ Util::Log::warning(' or ') ~ Util::Log::info('(n)o')
        ~ Util::Log::warning(': ');
      my Str:D \answer = $*IN.get;
      (answer.lc ~~ /^(y|yes)$/).defined;
    }

    # Ask the user to confirm the cleanup plan.
    our sub askForCleanupConfirmation(--> Bool:D) {
      say Util::Log::warning
        'Would you like to nuke the Nix profile generation symlinks according to the plan?';
      say
        Util::Log::warning('Will run “')
        ~ Util::Log::danger('sudo rm -- PROFILE_GENERATION_SYMLINK_PATH_HERE')
        ~ Util::Log::warning('” for each symlink.');

      my Bool:D \answer = yesOrNo;
      if answer {
        say Util::Log::success
          '[✓] Received a confirmation for the cleanup. Proceeding…';
        say
          Util::Log::warning('Note that you will probably be asked for your password for “')
          ~ Util::Log::danger('sudo')
          ~ Util::Log::warning('”.');
        ''.say
      } else {
        say Util::Log::danger
          '[✗] Did not receive a confirmation, not cleaning anything up, exiting…';
      }
      answer
    }

    our sub askForNixCollectGarbageRunConfirmation(--> Bool:D) {
      say
        Util::Log::warning('Would you like to run “')
        ~ Util::Log::info('nix-collect-garbage')
        ~ Util::Log::warning('”?');

      my Bool:D \answer = yesOrNo;
      if answer {
        say Util::Log::success '[✓] Received a confirmation for Nix GC run. Proceeding…';
        ''.say
      } else {
        say Util::Log::info '[✗] Did not receive a confirmation, skipping Nix GC run…';
      }
    }

    our sub reportEverythingIsClean() {
      say Util::Log::success '[✓] You have nothing to clean up. Everything is clean already.';
    }

    our sub debugSubProcCall(Str:D @cmd) {
      my Str:D \shellCmd =
        @cmd.map({
          if $_ !~~ m{^<[- _ a..z A..Z 0..9 . /]>+$} {
            # This is not a proper shell escaping, but should be good enough for this scirpt.
            "'" ~ $_ ~ "'"
          } else {
            $_
          }
        }).join(' ');

      say Util::Log::info('Running “') ~ Util::Log::warning(shellCmd) ~ Util::Log::info('”…');
    }
  }

  package SubProc {
    our sub sudoRm(IO::Path:D \file) {
      my NonEmptyStr:D @cmd = « sudo rm -- "{file.absolute}" »;
      Log::debugSubProcCall(@cmd);
      run(|@cmd);
    }

    our sub nixGcRun() {
      my NonEmptyStr:D @cmd = « nix-collect-garbage »;
      Log::debugSubProcCall(@cmd);
      run(|@cmd);
    }
  }

  # Make cleanup plan for cleaning the profile generations symlinks.
  #
  # “nixosReleasesNotToPreserve” allows to you clean up older NixOS releases.
  # By default the plan preserves at least latest build of each NixOS release,
  # only cleaning up older per-release generations.
  our sub makePlan(
    Hash[ProfileSymlink::Types::Profile:D] \profiles,
    ProfileSymlink::Types::NixosVersionList:D \nixosReleasesNotToPreserve = []
    --> Types::ProfilesCleanupPlanMap:D
  ) {
    my Types::ProfilesCleanupPlanMap:D \plan = %();

    # Generation symlinks cleanup plan mapping (symlink -> to nuke or to keep).
    my subset GenPlan where (
      Types::GenerationSymlink:D, # Key-ish
      Bool:D, # Value-ish (the Bool answers: are we keeping it?)
    );

    # Numerically sort a list of generation symlinks by generation number extracted from the
    # symlink’s file name. Descending order.
    my sub sortGens(Types::GenerationSymlinks:D \gens --> Types::GenerationSymlinks:D) {
      gens.sort({
        ProfileSymlink::Parse::GenerationFileName.parse(
          $_.Hash<profileSymlink>.basename
        )<generationNum>.UInt
      }).reverse.List
    }

    for profiles.kv -> NonEmptyStr:D \k, ProfileSymlink::Types::Profile:D \profile {
      my Types::NixosVersionMap:D \nixosVersions = Types::NixosVersionMap.new;
      my IO::Path:D \currentGeneration = profile.Hash<profile>.Hash<pointsTo>;
      my GenPlan:D @gensPlan; # See the comment for the “GenPlan” type

      for profile.Hash<generations>.List -> ProfileSymlink::Types::Generation:D \gen {
        my Types::GenerationSymlink:D \genSymlink =
          gen.Hash:p.grep({
            .key ~~ 'profileSymlink' | 'pointsTo' | 'systemProfilePreciseVersion'
          }).sort.List;

        my Bool:D \isKept = gen.Hash<profileSymlink>.absolute eq currentGeneration.absolute;

        # Add generation to the plan branching (to keep or to nuke). There are all generations of
        # this profile here paired with a Bool that decides whether to keep it or to nuke it.
        @gensPlan.push: $($(genSymlink), isKept);

        given gen.Hash<systemProfileVersion> {
          when .defined {
            # Add generation to a list of specific NixOS version
            nixosVersions.add: $_, genSymlink
          }
        }
      }

      # Keep one latest generation of each NixOS version.
      for nixosVersions.kv
      -> ProfileSymlink::Types::NixosVersion:D \k
      , Types::GenerationSymlinks:D \v {
        next if nixosReleasesNotToPreserve.grep(*~~k).elems > 0;
        my Types::GenerationSymlink:D \latest = v.&sortGens[0];
        for 0..(@gensPlan.elems-1) -> UInt:D \i {
          my GenPlan:D \x = @gensPlan[i];
          @gensPlan[i] = (x[0], True) if x[0] eqv latest
        }
      }

      plan{k} = (
        profileName => profile.Hash<profile>.Hash<profileName>,
        currentProfile => profile.Hash<profile>,
        generationsToKeep => @gensPlan.grep(*[1].so).map(*[0]).List.&sortGens,
        generationsToNuke => @gensPlan.grep(*[1].so.not).map(*[0]).List.&sortGens,
        systemNixosVersions => nixosVersions.elems > 0 ?? nixosVersions !! Nil,
      ).sort.List;
    }

    plan
  }

  # Collect all symlinks to delete from all profiles
  our sub symlinksToDelete(Types::ProfilesCleanupPlanMap:D \plan --> Types::GenerationSymlinks:D) {
    # Preserve order like it’s shown in the cleanup plan (order by profile key first, the symlinks
    # are already sorted by generation number in descending order).
    plan.keys.sort.map({plan{$($_)}}).map(*.Hash<generationsToNuke>.map({ $($_) })).flat.List
  }
}

# NixOS releases to nuke value coming from command-line arguments.
subset NukeReleasesStr where Nil | NonEmptyStr:D;

# Convert command-line arguments value to “ProfileSymlink::Types::NixosVersionList:D”.
sub getNixReleasesToNuke(NukeReleasesStr \releasesStr --> ProfileSymlink::Types::NixosVersionList:D) {
  return [] if releasesStr ~~ Nil;
  releasesStr.split(/\s*','\s*/).map({ ProfileSymlink::Parse::parseNixosRelease $_ }).Array
}

#| `--nuke-releases` accepts one or more NixOS release versions separated by
#| comma (e.g. `23.05,23.11`). Note that if you add a release that matches
#| current profile it won’t be nuked.
#| `--nuke-user-home-profiles` is useful for cleaning up Home Manager profiles.
sub MAIN(NukeReleasesStr :$nuke-releases = Nil, Bool:D :$nuke-user-home-profiles = False) {
  my ProfileSymlink::Types::NixosVersionList:D \nixReleasesToNuke =
    getNixReleasesToNuke $nuke-releases;

  my ProfileSymlink::Types::Profile:D %profiles =
    ProfileSymlink::getDirectoryProfiles nixProfilesRootDir;

  if $nuke-user-home-profiles {
    my ProfileSymlink::Types::Profile:D %userHomeProfiles =
      ProfileSymlink::getDirectoryProfiles userHomeNixProfilesRootDir;
    %profiles.append(%userHomeProfiles);
  }

  my Cleanup::Types::ProfilesCleanupPlanMap:D \cleanupPlan =
    Cleanup::makePlan %profiles, nixReleasesToNuke;

  Cleanup::Log::renderPlan(cleanupPlan).say;

  my IO::Path:D @filesToDelete = Cleanup::symlinksToDelete(cleanupPlan).map(*.Hash<profileSymlink>);

  if @filesToDelete.elems < 1 {
    Cleanup::Log::reportEverythingIsClean;
    exit 0
  }

  exit 1 unless Cleanup::Log::askForCleanupConfirmation();
  Cleanup::SubProc::sudoRm $_ for @filesToDelete;
  ''.say;

  exit 0 unless Cleanup::Log::askForNixCollectGarbageRunConfirmation();
  Cleanup::SubProc::nixGcRun;
}

# FIXME: Rakudo type-checker bug #5508.
#        When you use a custom “subset” type for a hash argument in a function like this:
#        “CustomSubsetType:D %foo” Rakudo fails to type-check it. It’s Rakudo’s bug.
#        See: https://github.com/rakudo/rakudo/issues/5508
#        The workaround is to use sigilless variable utilizing “Hash[…]” type, like this:
#        “Hash[CustomSubsetType:D] \foo”. You can find this pattern in this script. As soon as this
#        bug is fixed in a stable Rakudo release change it back to a “CustomSubsetType:D %foo”
#        pattern.
