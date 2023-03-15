# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# I wanted to try it for configuring USBStreamer but it turned out it’s not in the list of supported
# devices. Just keeping it here as it’s a buildable derivation. Maybe will send a merge request to
# nixpkgs later.

{ lib, stdenv, fetchFromGitHub, rustPlatform
, pkgconfig
, libusb
}:

rustPlatform.buildRustPackage rec {
  pname = "minidsp-rs";
  version = "dev-branch-post-0.1.9";

  src = fetchFromGitHub {
    owner = "mrene";
    repo = "minidsp-rs";
    # rev = "v${version}";
    rev = "094e8717b306c7e2e239f26059b4bb0495d557c4"; # “dev” branch, 14 Feb 2023
    sha256 = "sha256-nu214hO66A5WZu8emfVZXQKFRxyX2xBYimx1sZY5NbU=";
  };

  cargoHash = "sha256-gackG6Dos/27i8+5Ssle6DC0e5YaU2CNuxd4r76tlhc=";

  meta = with lib; {
    description = "minidsp-rs is an alternative control software for certain MiniDSP products";
    homepage = "https://minidsp-rs.pages.dev/";
    license = licenses.apsl20;
    maintainers = with maintainers; [ unclechu ];
  };

  nativeBuildInputs = [
    pkgconfig
  ];

  buildInputs = [
    libusb
  ];
}
