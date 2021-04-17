# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ ... }:
{
  networking = {
    networkmanager.enable = true;
    # wireless.enable = true; # incompatible with networkmanager
    useDHCP = false; # just legacy flag

    # let network manager do the work.
    # if you turn both this and network manager on the network
    # will constantly go up and down in an infinite loop.
    # interfaces.enp3s0.useDHCP = false;

    # proxy = {
    #   default = "http://user:password@proxy:port/";
    #   noProxy = "127.0.0.1,localhost,internal.domain";
    # };

    firewall = {
      enable = true;
      allowedTCPPorts = [
        # 80 443
      ];
      allowedTCPPortRanges = [
        # { from = 8000; to = 8010; }
      ];
    };
  };
}
