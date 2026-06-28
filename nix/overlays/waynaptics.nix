final: prev:
let
  src = prev.fetchFromGitHub {
    fetchSubmodules = true;
    hash = "sha256-UtQx/ky85KbxpV7I1VvLTKe/AdT/4kaz4QTUbBUg/zM=";
    owner = "kekekeks";
    repo = "waynaptics";
    rev = "v${version}";
  };
  version = "0.10.0";
in
{
  waynaptics = prev.stdenv.mkDerivation {
    inherit src version;
    pname = "waynaptics";

    buildInputs = with prev; [
      glib
      libevdev
      libX11
    ];
    cmakeFlags = [
      "-DBUILD_CONFIG_TOOL=OFF"
      "-DCMAKE_C_FLAGS=-Wno-incompatible-pointer-types"
    ];
    nativeBuildInputs = with prev; [
      cmake
      pkg-config
    ];
  };
}
