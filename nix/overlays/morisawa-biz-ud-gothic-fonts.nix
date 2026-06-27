final: prev: {
  morisawa-biz-ud-gothic-fonts = prev.stdenv.mkDerivation rec {
    pname = "morisawa-biz-ud-gothic-fonts";
    version = "1.051";
    src = prev.fetchzip {
      hash = "sha256-7PlIrQX1fnFHXm7mjfoOCVp3GSnLT2GlVZdSoZbh/s4=";
      url = "https://github.com/googlefonts/morisawa-biz-ud-gothic/releases/download/v${version}/morisawa-biz-ud-gothic-fonts.zip";
    };
    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      cp fonts/ttf/*.ttf $out/share/fonts/truetype
    '';
  };
}
