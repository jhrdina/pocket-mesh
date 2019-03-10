type key;
type jwk;
let fingerprintForRSAJWK: jwk => string;
let stringToJwk: string => option(jwk);
let jwkToPublicKey: jwk => option(key);
let verify: (key, string, string) => bool;