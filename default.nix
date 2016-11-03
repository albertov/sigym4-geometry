{ mkDerivation, aeson, base, binary, bytestring
, data-binary-ieee754, deepseq, distributive, hashable, hspec, lens
, linear, mtl, primitive, QuickCheck, semigroups, spatial-reference
, stdenv, strict, template-haskell, text, transformers
, unordered-containers, vector, vector-th-unbox
}:
mkDerivation {
  pname = "sigym4-geometry";
  version = "0.4.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring data-binary-ieee754 deepseq
    distributive hashable lens linear mtl primitive semigroups
    spatial-reference strict template-haskell text transformers
    unordered-containers vector vector-th-unbox
  ];
  testHaskellDepends = [
    base bytestring deepseq hspec QuickCheck vector
  ];
  homepage = "https://github.com/albertov/sigym4-geometry";
  description = "Geometry types for GIS applications";
  license = stdenv.lib.licenses.bsd3;
}
