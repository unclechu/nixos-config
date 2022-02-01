# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:

pkgs.bibata-cursors.overrideAttrs (srcAttrs: {
  nativeBuildInputs = srcAttrs.nativeBuildInputs ++ [
    pkgs.findutils
    pkgs.gnugrep
    pkgs.perl
    pkgs.imagemagick
  ];

  patches = [
    ./flip-x-hot-points-for-left-handed-cursors.patch
  ];

  buildPhase = ''
    mkdir bitmaps
    unzip $bitmaps -d bitmaps
    rm -rf themes

    (
      set -uo pipefail || exit

      images_to_flip=$(
        cat builder/src/constants.py \
          | perl -nE 'if (/^horizontally_flipped/) {$x=1;next}; print if $x' \
          | perl -nE '$x=1 if /^]/; print unless $x' \
          | sed -e 's/#.*//' -e 's/[," ]//g' \
          | grep -v '^$'
      )

      static_images_to_flip=$(grep '\.png$' <<< "$images_to_flip")
      animated_images_to_flip=$(grep -v '\.png$' <<< "$images_to_flip")
      readarray -t static_images_to_flip <<< "$static_images_to_flip"
      readarray -t animated_images_to_flip <<< "$animated_images_to_flip"

      cd bitmaps
      themes=$(ls)
      readarray -t themes <<< "$themes"

      for theme in "''${themes[@]}"; do
        (
          cd -- "$theme"

          >&2 printf 'Horizontally flipping static images for "%s" theme...\n' "$theme"
          for image in "''${static_images_to_flip[@]}"; do
            (set -x && convert -flop -- "$image" "$image")
          done

          >&2 printf 'Horizontally flipping animated frames for "%s" theme...\n' "$theme"
          for file_prefix in "''${animated_images_to_flip[@]}"; do
            files=$(find -name "''${file_prefix}*.png")
            readarray -t files <<< "$files"
            for image in "''${files[@]}"; do
              (set -x && convert -flop -- "$image" "$image")
            done
          done
        )
      done
    )

    cd builder && make build_unix
  '';
})
