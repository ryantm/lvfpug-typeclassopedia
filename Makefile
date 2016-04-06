all :
	nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.doctest pkgs.QuickCheck])"
