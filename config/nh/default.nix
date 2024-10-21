{ config, options, lib, pkgs, ... }: let 
	inherit (lib) mkIf mkOption types platforms;
	inherit (lib.hm) assertions; 
	cfg = config.programs.nh;
in {
	options = {
		programs.nh = {
			enable = mkOption {
			type = types.bool; 
			default = false;
			description = ''
				Enable nh
		'';
		};
		};
	};

	config = mkIf cfg.enable {
		assertions = [ (assertions.assertPlatform "programs.nh" pkgs platforms.linux)];
		home.packages = [
			pkgs.nh
		];
	};
}
