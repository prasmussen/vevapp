{ pkgs ? import <nixpkgs> {} }:

pkgs.buildGoModule rec {
  name = "dict-backend-${version}";
  version = "1.0.0";

  src = ./src;

  modSha256 = "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5";

  vendorSha256 = "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5";

  subPackages = [ "." ];

  meta = with pkgs.lib; {
    description = "dict-backend";
    homepage = https://github.com/prasmussen/vevapp;
    license = licenses.mit;
    maintainers = with maintainers; [ prasmussen ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
