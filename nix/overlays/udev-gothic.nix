final: prev: {
  udev-gothic = prev.stdenv.mkDerivation rec {
    pname = "udev-gothic";
    version = "2.2.0";
    src = prev.fetchzip {
      hash = "sha256-x6nM35UM7v4WQn6DINuEgXQmSQ4ysPS4omY9ePDTAhA=";
      url = "https://github.com/yuru7/udev-gothic/releases/download/v${version}/UDEVGothic_NF_v${version}.zip";
    };
    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      cp *.ttf $out/share/fonts/truetype
    '';
  };
}
