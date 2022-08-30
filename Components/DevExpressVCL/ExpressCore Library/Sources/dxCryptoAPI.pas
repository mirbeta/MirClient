{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxCryptoAPI;

{$I cxVer.inc}

interface

uses
  Windows;

const
  CryptoProviderEnhancedRSA_AES = 'Microsoft Enhanced RSA and AES Cryptographic Provider';
  CryptoProviderEnhancedRSA_AES_XP = 'Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)';

const
  PROV_RSA_FULL      = 1;
  {$EXTERNALSYM PROV_RSA_FULL}
  PROV_RSA_SIG       = 2;
  {$EXTERNALSYM PROV_RSA_SIG}
  PROV_RSA_AES       = 24;
  {$EXTERNALSYM PROV_RSA_AES}

type
  HCRYPTPROV = ULONG_PTR;
  {$EXTERNALSYM HCRYPTPROV}
  HCRYPTKEY = ULONG_PTR;
  {$EXTERNALSYM HCRYPTKEY}
  HCRYPTHASH = ULONG_PTR;
  {$EXTERNALSYM HCRYPTHASH}

  PHCRYPTPROV = ^HCRYPTPROV;
  {$NODEFINE PHCRYPTPROV}
  PHCRYPTKEY = ^HCRYPTKEY;
  {$NODEFINE PHCRYPTKEY}
  PHCRYPTHASH = ^HCRYPTHASH;
  {$NODEFINE PHCRYPTHASH}

const
  CRYPT_VERIFYCONTEXT  = DWORD($F0000000);
  {$EXTERNALSYM CRYPT_VERIFYCONTEXT}
  CRYPT_NEWKEYSET      = $00000008;
  {$EXTERNALSYM CRYPT_NEWKEYSET}
  CRYPT_DELETEKEYSET   = $00000010;
  {$EXTERNALSYM CRYPT_DELETEKEYSET}
  CRYPT_MACHINE_KEYSET = $00000020;
  {$EXTERNALSYM CRYPT_MACHINE_KEYSET}
  CRYPT_SILENT         = $00000040;
  {$EXTERNALSYM CRYPT_SILENT}

type
  ALG_ID = Cardinal;
  {$EXTERNALSYM ALG_ID}

const
  CALG_3DES	          = $00006603;
  {$EXTERNALSYM CALG_3DES}
  CALG_3DES_112	      = $00006609;
  {$EXTERNALSYM CALG_3DES_112}
  CALG_AES            = $00006611;
  {$EXTERNALSYM CALG_AES}
  CALG_AES_128        = $0000660e;
  {$EXTERNALSYM CALG_AES_128}
  CALG_AES_192        = $0000660f;
  {$EXTERNALSYM CALG_AES_192}
  CALG_AES_256        = $00006610;
  {$EXTERNALSYM CALG_AES_256}
  CALG_AGREEDKEY_ANY  = $0000aa03;
  {$EXTERNALSYM CALG_AGREEDKEY_ANY}
  CALG_CYLINK_MEK	    = $0000660c;
  {$EXTERNALSYM CALG_CYLINK_MEK}
  CALG_DES	          = $00006601;
  {$EXTERNALSYM CALG_DES}
  CALG_DESX	          = $00006604;
  {$EXTERNALSYM CALG_DESX}
  CALG_DH_EPHEM	      = $0000aa02;
  {$EXTERNALSYM CALG_DH_EPHEM}
  CALG_DH_SF	        = $0000aa01;
  {$EXTERNALSYM CALG_DH_SF}
  CALG_DSS_SIGN	      = $00002200;
  {$EXTERNALSYM CALG_DSS_SIGN}

  CALG_HASH_REPLACE_OWF	= $0000800b;
  {$EXTERNALSYM CALG_HASH_REPLACE_OWF}
  CALG_HUGHES_MD5   	= $0000a003;
  {$EXTERNALSYM CALG_HUGHES_MD5}
  CALG_HMAC           = $00008009;
  {$EXTERNALSYM CALG_HMAC}
  CALG_MAC	          = $00008005;
  {$EXTERNALSYM CALG_MAC}
  CALG_MD2	          = $00008001;
  {$EXTERNALSYM CALG_MD2}
  CALG_MD4          	= $00008002;
  {$EXTERNALSYM CALG_MD4}
  CALG_MD5	          = $00008003;
  {$EXTERNALSYM CALG_MD5}
  CALG_NO_SIGN	      = $00002000;
  {$EXTERNALSYM CALG_NO_SIGN}
  CALG_RC2	          = $00006602;
  {$EXTERNALSYM CALG_RC2}
  CALG_RC4	          = $00006801;
  {$EXTERNALSYM CALG_RC4}
  CALG_RC5	          = $0000660d;
  {$EXTERNALSYM CALG_RC5}
  CALG_RSA_KEYX	      = $0000a400;
  {$EXTERNALSYM CALG_RSA_KEYX}
  CALG_RSA_SIGN	      = $00002400;
  {$EXTERNALSYM CALG_RSA_SIGN}
  CALG_SHA	          = $00008004;
  {$EXTERNALSYM CALG_SHA}
  CALG_SHA1         	= $00008004;
  {$EXTERNALSYM CALG_SHA1}
  CALG_SHA_256	      = $0000800c;
  {$EXTERNALSYM CALG_SHA_256}
  CALG_SHA_384	      = $0000800d;
  {$EXTERNALSYM CALG_SHA_384}
  CALG_SHA_512	      = $0000800e;
  {$EXTERNALSYM CALG_SHA_512}

  CALG_ECDH	          = $0000aa05;
  {$EXTERNALSYM CALG_ECDH}
  CALG_ECDH_EPHEM	    = $0000ae06;
  {$EXTERNALSYM CALG_ECDH_EPHEM}
  CALG_ECDSA	        = $00002203;
  {$EXTERNALSYM CALG_ECDSA}

const
  HP_ALGID         = $0001; // Hash algorithm
  {$EXTERNALSYM HP_ALGID}
  HP_HASHVAL       = $0002; // Hash value
  {$EXTERNALSYM HP_HASHVAL}
  HP_HASHSIZE      = $0004; // Hash value size
  {$EXTERNALSYM HP_HASHSIZE}
  HP_HMAC_INFO     = $0005; // information for creating an HMAC
  {$EXTERNALSYM HP_HMAC_INFO}
  HP_TLS1PRF_LABEL = $0006; // label for TLS1 PRF
  {$EXTERNALSYM HP_TLS1PRF_LABEL}
  HP_TLS1PRF_SEED  = $0007; // seed for TLS1 PRF
  {$EXTERNALSYM HP_TLS1PRF_SEED}

// dwParam

  KP_IV               = 1; // Initialization vector
  {$EXTERNALSYM KP_IV}
  KP_SALT             = 2; // Salt value
  {$EXTERNALSYM KP_SALT}
  KP_PADDING          = 3; // Padding values
  {$EXTERNALSYM KP_PADDING}
  KP_MODE             = 4; // Mode of the cipher
  {$EXTERNALSYM KP_MODE}
  KP_MODE_BITS        = 5; // Number of bits to feedback
  {$EXTERNALSYM KP_MODE_BITS}
  KP_PERMISSIONS      = 6; // Key permissions DWORD
  {$EXTERNALSYM KP_PERMISSIONS}
  KP_ALGID            = 7; // Key algorithm
  {$EXTERNALSYM KP_ALGID}
  KP_BLOCKLEN         = 8; // Block size of the cipher
  {$EXTERNALSYM KP_BLOCKLEN}
  KP_KEYLEN           = 9; // Length of key in bits
  {$EXTERNALSYM KP_KEYLEN}
  KP_SALT_EX          = 10; // Length of salt in bytes
  {$EXTERNALSYM KP_SALT_EX}
  KP_P                = 11; // DSS/Diffie-Hellman P value
  {$EXTERNALSYM KP_P}
  KP_G                = 12; // DSS/Diffie-Hellman G value
  {$EXTERNALSYM KP_G}
  KP_Q                = 13; // DSS Q value
  {$EXTERNALSYM KP_Q}
  KP_X                = 14; // Diffie-Hellman X value
  {$EXTERNALSYM KP_X}
  KP_Y                = 15; // Y value
  {$EXTERNALSYM KP_Y}
  KP_RA               = 16; // Fortezza RA value
  {$EXTERNALSYM KP_RA}
  KP_RB               = 17; // Fortezza RB value
  {$EXTERNALSYM KP_RB}
  KP_INFO             = 18; // for putting information into an RSA envelope
  {$EXTERNALSYM KP_INFO}
  KP_EFFECTIVE_KEYLEN = 19; // setting and getting RC2 effective key length
  {$EXTERNALSYM KP_EFFECTIVE_KEYLEN}
  KP_SCHANNEL_ALG     = 20; // for setting the Secure Channel algorithms
  {$EXTERNALSYM KP_SCHANNEL_ALG}
  KP_CLIENT_RANDOM    = 21; // for setting the Secure Channel client random data
  {$EXTERNALSYM KP_CLIENT_RANDOM}
  KP_SERVER_RANDOM    = 22; // for setting the Secure Channel server random data
  {$EXTERNALSYM KP_SERVER_RANDOM}
  KP_RP               = 23;
  {$EXTERNALSYM KP_RP}
  KP_PRECOMP_MD5      = 24;
  {$EXTERNALSYM KP_PRECOMP_MD5}
  KP_PRECOMP_SHA      = 25;
  {$EXTERNALSYM KP_PRECOMP_SHA}
  KP_CERTIFICATE      = 26; // for setting Secure Channel certificate data (PCT1)
  {$EXTERNALSYM KP_CERTIFICATE}
  KP_CLEAR_KEY        = 27; // for setting Secure Channel clear key data (PCT1)
  {$EXTERNALSYM KP_CLEAR_KEY}
  KP_PUB_EX_LEN       = 28;
  {$EXTERNALSYM KP_PUB_EX_LEN}
  KP_PUB_EX_VAL       = 29;
  {$EXTERNALSYM KP_PUB_EX_VAL}
  KP_KEYVAL           = 30;
  {$EXTERNALSYM KP_KEYVAL}
  KP_ADMIN_PIN        = 31;
  {$EXTERNALSYM KP_ADMIN_PIN}
  KP_KEYEXCHANGE_PIN  = 32;
  {$EXTERNALSYM KP_KEYEXCHANGE_PIN}
  KP_SIGNATURE_PIN    = 33;
  {$EXTERNALSYM KP_SIGNATURE_PIN}
  KP_PREHASH          = 34;
  {$EXTERNALSYM KP_PREHASH}

  KP_OAEP_PARAMS     = 36; // for setting OAEP params on RSA keys
  {$EXTERNALSYM KP_OAEP_PARAMS}
  KP_CMS_KEY_INFO    = 37;
  {$EXTERNALSYM KP_CMS_KEY_INFO}
  KP_CMS_DH_KEY_INFO = 38;
  {$EXTERNALSYM KP_CMS_DH_KEY_INFO}
  KP_PUB_PARAMS      = 39; // for setting public parameters
  {$EXTERNALSYM KP_PUB_PARAMS}
  KP_VERIFY_PARAMS   = 40; // for verifying DSA and DH parameters
  {$EXTERNALSYM KP_VERIFY_PARAMS}
  KP_HIGHEST_VERSION = 41; // for TLS protocol version setting
  {$EXTERNALSYM KP_HIGHEST_VERSION}

// KP_PADDING

  PKCS5_PADDING  = 1; // PKCS 5 (sec 6.2) padding method
  {$EXTERNALSYM PKCS5_PADDING}
  RANDOM_PADDING = 2;
  {$EXTERNALSYM RANDOM_PADDING}
  ZERO_PADDING   = 3;
  {$EXTERNALSYM ZERO_PADDING}

// KP_MODE

  CRYPT_MODE_CBC = 1; // Cipher block chaining
  {$EXTERNALSYM CRYPT_MODE_CBC}
  CRYPT_MODE_ECB = 2; // Electronic code book
  {$EXTERNALSYM CRYPT_MODE_ECB}
  CRYPT_MODE_OFB = 3; // Output feedback mode
  {$EXTERNALSYM CRYPT_MODE_OFB}
  CRYPT_MODE_CFB = 4; // Cipher feedback mode
  {$EXTERNALSYM CRYPT_MODE_CFB}
  CRYPT_MODE_CTS = 5; // CipherText stealing mode
  {$EXTERNALSYM CRYPT_MODE_CTS}

// KP_PERMISSIONS

  CRYPT_ENCRYPT    = $0001; // Allow encryption
  {$EXTERNALSYM CRYPT_ENCRYPT}
  CRYPT_DECRYPT    = $0002; // Allow decryption
  {$EXTERNALSYM CRYPT_DECRYPT}
  CRYPT_EXPORT     = $0004; // Allow key to be exported
  {$EXTERNALSYM CRYPT_EXPORT}
  CRYPT_READ       = $0008; // Allow parameters to be read
  {$EXTERNALSYM CRYPT_READ}
  CRYPT_WRITE      = $0010; // Allow parameters to be set
  {$EXTERNALSYM CRYPT_WRITE}
  CRYPT_MAC        = $0020; // Allow MACs to be used with key
  {$EXTERNALSYM CRYPT_MAC}
  CRYPT_EXPORT_KEY = $0040; // Allow key to be used for exporting keys
  {$EXTERNALSYM CRYPT_EXPORT_KEY}
  CRYPT_IMPORT_KEY = $0080; // Allow key to be used for importing keys
  {$EXTERNALSYM CRYPT_IMPORT_KEY}

// exported key blob definitions

  SIMPLEBLOB           = $1;
  {$EXTERNALSYM SIMPLEBLOB}
  PUBLICKEYBLOB        = $6;
  {$EXTERNALSYM PUBLICKEYBLOB}
  PRIVATEKEYBLOB       = $7;
  {$EXTERNALSYM PRIVATEKEYBLOB}
  PLAINTEXTKEYBLOB     = $8;
  {$EXTERNALSYM PLAINTEXTKEYBLOB}
  OPAQUEKEYBLOB        = $9;
  {$EXTERNALSYM OPAQUEKEYBLOB}
  PUBLICKEYBLOBEX      = $A;
  {$EXTERNALSYM PUBLICKEYBLOBEX}
  SYMMETRICWRAPKEYBLOB = $B;
  {$EXTERNALSYM SYMMETRICWRAPKEYBLOB}

  AT_KEYEXCHANGE = 1;
  {$EXTERNALSYM AT_KEYEXCHANGE}
  AT_SIGNATURE   = 2;
  {$EXTERNALSYM AT_SIGNATURE}

  CRYPT_USERDATA = 1;
  {$EXTERNALSYM CRYPT_USERDATA}

  CUR_BLOB_VERSION = 2;
  {$EXTERNALSYM CUR_BLOB_VERSION}

type
  PdxPublicKeyStructure = ^TdxPublicKeyStructure;
  TdxPublicKeyStructure = record
    bType: BYTE;
    bVersion: BYTE;
    reserved: WORD;
    aiKeyAlg: ALG_ID;
  end;

  PdxHMACInfo = ^TdxHMACInfo;
  TdxHMACInfo = record
    HashAlgid: ALG_ID;
    pbInnerString: PBYTE;
    cbInnerString: DWORD;
    pbOuterString: PBYTE;
    cbOuterString: DWORD;
  end;

function CryptAcquireContext(var phProv: HCRYPTPROV; pszContainer: LPCTSTR;
  pszProvider: LPCTSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptAcquireContext}
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: ULONG_PTR): BOOL; stdcall;
{$EXTERNALSYM CryptReleaseContext}

function CryptCreateHash(hProv: HCRYPTPROV; Algid: ALG_ID; hKey: HCRYPTKEY; dwFlags: DWORD; var phHash: HCRYPTHASH): BOOL; stdcall;
{$EXTERNALSYM CryptCreateHash}
function CryptHashData(hHash: HCRYPTHASH; pbData: LPBYTE; dwDataLen, dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptHashData}
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;
{$EXTERNALSYM CryptDestroyHash}
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: LPBYTE; var pdwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptGetHashParam}
function CryptSetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: LPBYTE; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetHashParam}

function CryptGetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: LPBYTE; var pdwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptGetKeyParam}
function CryptDuplicateKey(hKey: HCRYPTKEY; pdwReserved: LPDWORD; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptDuplicateKey}
function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptDestroyKey}
function CryptImportKey(hProv: HCRYPTPROV; pbData: LPBYTE; dwDataLen: DWORD; hPubKey: HCRYPTKEY; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptImportKey}
function CryptDeriveKey(hProv: HCRYPTPROV; Algid: ALG_ID; hBaseData: HCRYPTHASH; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
{$EXTERNALSYM CryptDeriveKey}
function CryptSetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: LPBYTE; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetKeyParam}

function CryptDecrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL; dwFlags: DWORD; pbData: LPBYTE; var pdwDataLen: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptDecrypt}
function CryptEncrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL; dwFlags: DWORD; pbData: LPBYTE; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptEncrypt}

function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: LPBYTE): BOOL; stdcall;
{$EXTERNALSYM CryptGenRandom}

procedure CryptCheck(AResult: LongBool);
implementation

uses
  SysUtils;

function CryptAcquireContext; external advapi32 name 'CryptAcquireContextW';
function CryptReleaseContext; external advapi32 name 'CryptReleaseContext';

function CryptCreateHash; external advapi32 name 'CryptCreateHash';
function CryptDestroyHash; external advapi32 name 'CryptDestroyHash';
function CryptGetHashParam; external advapi32 name 'CryptGetHashParam';
function CryptHashData; external advapi32 name 'CryptHashData';
function CryptSetHashParam; external advapi32 name 'CryptSetHashParam';

function CryptDeriveKey; external advapi32 name 'CryptDeriveKey';
function CryptDestroyKey; external advapi32 name 'CryptDestroyKey';
function CryptDuplicateKey; external advapi32 name 'CryptDuplicateKey';
function CryptGetKeyParam; external advapi32 name 'CryptGetKeyParam';
function CryptImportKey; external advapi32 name 'CryptImportKey';
function CryptSetKeyParam; external advapi32 name 'CryptSetKeyParam';

function CryptDecrypt; external advapi32 name 'CryptDecrypt';
function CryptEncrypt; external advapi32 name 'CryptEncrypt';
function CryptGenRandom; external advapi32 name 'CryptEncrypt';

procedure CryptCheck(AResult: LongBool);
begin
  if not AResult then
    RaiseLastOSError;
end;

end.
