{*******************************************************}
{                                                       }
{                Certificate routines                   }
{                                                       }
{        Copyright (c) 2013-2018 Michal Mutl            }
{                                                       }
{*******************************************************}

{$I Compilers.Inc}

unit MiTeC_Cert;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_Windows;

type
  TCertInfo = record
    Version: Integer;
    Issuer,
    Subject,
    Serial,
    SignatureAlgorithm,
    PublicKeyAlgorithm: string;
    PublicKeyBits: Integer;
    ValidFrom,
    ValidTo: TDateTime;
  end;

function GetCertInfo(const AFilename: string; out ACertInfo: TCertInfo): Boolean; overload;
function GetCertInfo(AStream: TStream; out ACertInfo: TCertInfo): Boolean; overload;
function GetPFXInfo(const AFilename, APassword: string; out ACertInfo: TCertInfo): Boolean;
function VerifyFile(const AFilename: string; var ASigner: string; var ATimestamp: TDatetime; AHandle: THandle = INVALID_HANDLE_VALUE): Integer; overload;
function VerifyFile(const AFilename: string; AHandle: THandle = INVALID_HANDLE_VALUE): Integer; overload;
procedure ViewPFXCertificate(AHandle: THandle; const AFilename, APassword: string);
procedure ViewCertificate(AHandle: THandle; const AFilename: string);
function GetCertErrorText(ACode: integer): string;

implementation

uses MiTeC_WinCrypt, MiTeC_WinTrust, MiTeC_Datetime, MiTeC_CryptUI;

function BLOBToStr(ABLOB: DATA_BLOB): string;
var
  n: Cardinal;
begin
  n:=CertNameToStr(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,@ABLOB,CERT_SIMPLE_NAME_STR,nil,0);
  SetString(Result,PChar(ABLOB.pbData),n);
  CertNameToStr(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,@ABLOB,CERT_SIMPLE_NAME_STR,PChar(Result),n);
end;

function NormalizeSerial(const AValue: string): string;
var
  i,l: Integer;
begin
  Result:='';
  i:=1;
  l:=Length(AValue);
  while i<l do begin
    Result:=Result+Copy(AValue,l-i,2);
    Inc(i,2);
  end;
end;

function GetOIDInfo(AValue: string): string;
var
  oid: PCCRYPT_OID_INFO;
begin
  Result:=AValue;
  oid:=CryptFindOIDInfo(CRYPT_OID_INFO_OID_KEY,PAnsiChar(WideToAnsi(Avalue)),0);
  if Assigned(oid) then
    Result:=oid.pwszName;
end;

function GetCertContext(const AFilename: string; out ACertContext: PCCERT_CONTEXT): boolean; overload;
var
  cs: HCERTSTORE;
begin
  Result:=False;
  ACertContext:=nil;
  cs:=CertOpenStore(CERT_STORE_PROV_FILENAME,X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,0,CERT_STORE_OPEN_EXISTING_FLAG or CERT_STORE_READONLY_FLAG,PChar(AFilename));
  if Assigned(cs) then begin
    ACertContext:=CertEnumCertificatesInStore(cs,nil);
    Result:=Assigned(ACertContext);
    CertCloseStore(cs,0);
  end;
end;

function GetCertContext(AStream: TStream; out ACertContext: PCCERT_CONTEXT): boolean; overload;
var
  cs: HCERTSTORE;
  Blob: CRYPTOAPI_BLOB;
  b: TBytes;
begin
  Result:=False;
  AStream.Position:=0;
  Blob.cbData:=AStream.Size;
  SetLength(b,AStream.Size);
  AStream.Read(b[0],AStream.Size);
  Blob.pbData:=@b[0];
  ACertContext:=nil;
  cs:=CertOpenStore(CERT_STORE_PROV_PKCS7,X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,0,0,@Blob);
  if Assigned(cs) then begin
    ACertContext:=CertEnumCertificatesInStore(cs,nil);
    Result:=Assigned(ACertContext);
    CertCloseStore(cs,0);
  end;
end;

function GetCertContext2(const AFilename: string; out ACertContext: PCCERT_CONTEXT): boolean;
const
  cBegin = '-----BEGIN';
  cBeginCert = '-----BEGIN CERTIFICATE-----';
  cEndCert = '-----END CERTIFICATE-----';
  cBeginPKCS7 = '-----BEGIN PKCS7-----';
  cEndPKCS7 = '-----END PKCS7-----';
var
  cfh: THandle;
  certEncoded,certDecoded: TBytes;
  certEncodedSize,cfs,n: Cardinal;
  s: string;
begin
  Result:=False;
  ACertContext:=nil;
  cfh:=CreateFile(PChar(AFilename),GENERIC_READ,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  if (cfh=INVALID_HANDLE_VALUE) then
    Exit;
  try
    cfs:=GetFileSize(cfh,nil);
    SetLength(certEncoded,cfs);
    if not ReadFile(cfh,certEncoded[0],cfs,certEncodedSize,nil) then
      Exit;
    s:=string(PAnsiChar(certEncoded));
    if Pos(cBegin,s)=1 then begin
      if Pos(cBeginCert,s)=1 then
        s:=Trim(Copy(s,Length(cBeginCert)+1,Length(s)-Length(cBeginCert)-Length(cEndCert)))
      else if Pos(cBeginPKCS7,s)=1 then
        s:=Trim(Copy(s,Length(cBeginPKCS7)+1,Length(s)-Length(cBeginPKCS7)-Length(cEndPKCS7)))
      else
        Exit;
      CryptStringToBinary(PChar(s),Length(s),CRYPT_STRING_BASE64_ANY,nil,n,nil,nil);
      SetLength(certDecoded,n);
      CryptStringToBinary(PChar(s),Length(s),CRYPT_STRING_BASE64_ANY,@certDecoded[0],n,nil,nil);
    end else begin
      certDecoded:=certEncoded;
      n:=cfs;
    end;
    ACertContext:=CertCreateCertificateContext(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,@certDecoded[0],n);
    Result:=Assigned(ACertContext);
  finally
    CloseHandle(cfh);
  end;
end;

function GetPFXCertContext(const AFilename, APassword: string; out ACertContext: PCCERT_CONTEXT): boolean;
var
  cfh: THandle;
  certEncoded: TBytes;
  certEncodedSize,cfs: Cardinal;
  csh: HCERTSTORE;
  pfx: CRYPT_DATA_BLOB;
begin
  Result:=False;
  ACertContext:=nil;
  cfh:=CreateFile(PChar(AFilename),GENERIC_READ,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  if (cfh=INVALID_HANDLE_VALUE) then
    Exit;
  try
    cfs:=GetFileSize(cfh,nil);
    SetLength(certEncoded,cfs);
    if not ReadFile(cfh,certEncoded[0],cfs,certEncodedSize,nil) then
      Exit;
  finally
    CloseHandle(cfh);
  end;
  pfx.cbData:=cfs;
  pfx.pbData:=@certEncoded[0];
  csh:=PFXImportCertStore(@pfx,PWideChar(APassword),0);
  if Assigned(csh) then begin
    ACertContext:=CertFindCertificateInStore(csh,X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,0,CERT_FIND_HAS_PRIVATE_KEY,nil,nil);
    CertCloseStore(csh,0);
  end;
  Result:=Assigned(ACertContext);
end;

function GetCertInfo(const AFilename: string; out ACertInfo: TCertInfo): Boolean;
var
  cert: PCCERT_CONTEXT;
begin
  Result:=GetCertContext(AFilename,cert);
  if Assigned(cert) then begin
    ACertInfo.Version:=cert.pCertInfo.dwVersion+1;
    ACertInfo.Issuer:=BLOBToStr(cert.pCertInfo.Issuer);
    ACertInfo.Subject:=BLOBToStr(cert.pCertInfo.Subject);
    ACertInfo.SignatureAlgorithm:=GetOIDInfo(string(cert.pCertInfo.SignatureAlgorithm.pszObjId));
    ACertInfo.PublicKeyAlgorithm:=GetOIDInfo(string(cert.pCertInfo.SubjectPublicKeyInfo.Algorithm.pszObjId));
    ACertInfo.PublicKeyBits:=(cert.pCertInfo.SubjectPublicKeyInfo.PublicKey.cbData-14)*8;
    ACertInfo.Serial:=NormalizeSerial(BinaryToFormat(cert.pCertInfo.SerialNumber.pbData,cert.pCertInfo.SerialNumber.cbData,CRYPT_STRING_HEX));
    ACertInfo.ValidFrom:=FiletimeToDatetime(cert.pCertInfo.NotBefore,True);
    ACertInfo.ValidTo:=FiletimeToDatetime(cert.pCertInfo.NotAfter,True);
    CertFreeCertificateContext(cert);
  end;
end;

function GetCertInfo(AStream: TStream; out ACertInfo: TCertInfo): Boolean;
var
  cert: PCCERT_CONTEXT;
begin
  Result:=GetCertContext(AStream,cert);
  if Assigned(cert) then begin
    ACertInfo.Version:=cert.pCertInfo.dwVersion+1;
    ACertInfo.Issuer:=BLOBToStr(cert.pCertInfo.Issuer);
    ACertInfo.Subject:=BLOBToStr(cert.pCertInfo.Subject);
    ACertInfo.SignatureAlgorithm:=GetOIDInfo(string(cert.pCertInfo.SignatureAlgorithm.pszObjId));
    ACertInfo.PublicKeyAlgorithm:=GetOIDInfo(string(cert.pCertInfo.SubjectPublicKeyInfo.Algorithm.pszObjId));
    ACertInfo.PublicKeyBits:=(cert.pCertInfo.SubjectPublicKeyInfo.PublicKey.cbData-14)*8;
    ACertInfo.Serial:=NormalizeSerial(BinaryToFormat(cert.pCertInfo.SerialNumber.pbData,cert.pCertInfo.SerialNumber.cbData,CRYPT_STRING_HEX));
    ACertInfo.ValidFrom:=FiletimeToDatetime(cert.pCertInfo.NotBefore,True);
    ACertInfo.ValidTo:=FiletimeToDatetime(cert.pCertInfo.NotAfter,True);
    CertFreeCertificateContext(cert);
  end;
end;

function GetPFXInfo(const AFilename, APassword: string; out ACertInfo: TCertInfo): Boolean;
var
  cert: PCCERT_CONTEXT;
begin
  Result:=GetPFXCertContext(AFilename,APassword,cert);
  if Assigned(cert) then begin
    ACertInfo.Version:=cert.pCertInfo.dwVersion+1;
    ACertInfo.Issuer:=BLOBToStr(cert.pCertInfo.Issuer);
    ACertInfo.Subject:=BLOBToStr(cert.pCertInfo.Subject);
    ACertInfo.SignatureAlgorithm:=GetOIDInfo(string(cert.pCertInfo.SignatureAlgorithm.pszObjId));
    ACertInfo.PublicKeyAlgorithm:=GetOIDInfo(string(cert.pCertInfo.SubjectPublicKeyInfo.Algorithm.pszObjId));
    ACertInfo.PublicKeyBits:=(cert.pCertInfo.SubjectPublicKeyInfo.PublicKey.cbData-14)*8;
    ACertInfo.Serial:=NormalizeSerial(BinaryToFormat(cert.pCertInfo.SerialNumber.pbData,cert.pCertInfo.SerialNumber.cbData,CRYPT_STRING_HEX));
    ACertInfo.ValidFrom:=FiletimeToDatetime(cert.pCertInfo.NotBefore,True);
    ACertInfo.ValidTo:=FiletimeToDatetime(cert.pCertInfo.NotAfter,True);
    CertFreeCertificateContext(cert);
  end;
end;

function GetSigner(hWVTStateData: THANDLE): string;
var
  provider: PCRYPT_PROVIDER_DATA;
  signer: PCRYPT_PROVIDER_SGNR;
  cert: PCRYPT_PROVIDER_CERT;
  s: string;
  i: Integer;
begin
  provider:=WTHelperProvDataFromStateData(hWVTStateData);
  if not Assigned(provider) then
    Exit;
  signer:=WTHelperGetProvSignerFromChain(provider,0,False,0);
  if not Assigned(signer) then
    Exit;
  cert:=WTHelperGetProvCertFromChain(signer,0);
  if not Assigned(cert) then
    Exit;
  i:=CertGetNameString(cert.pCert,CERT_NAME_SIMPLE_DISPLAY_TYPE,0,nil,nil,0);
  SetLength(s,i);
  CertGetNameString(cert.pCert,CERT_NAME_SIMPLE_DISPLAY_TYPE,0,nil,@s[1],Length(s));
  Result:=Trim(s);
end;

function GetTimestamp(hWVTStateData: THANDLE): TDatetime;
var
  provider: PCRYPT_PROVIDER_DATA;
  signer: PCRYPT_PROVIDER_SGNR;
  i: Integer;
  ft: TFiletime;
  n: Cardinal;
begin
  Result:=0;
  provider:=WTHelperProvDataFromStateData(hWVTStateData);
  if not Assigned(provider) then
    Exit;
  signer:=WTHelperGetProvSignerFromChain(provider,0,False,0);
  if not Assigned(signer) or not Assigned(signer.pasCounterSigners) then
    Exit;
  for i:=0 to signer.pasCounterSigners.psSigner.AuthAttrs.cAttr-1 do begin
    if signer.pasCounterSigners.psSigner.AuthAttrs.rgAttr.pszObjId=szOID_RSA_signingTime then begin
      n:=sizeof(ft);
      if CryptDecodeObject(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
                           szOID_RSA_signingTime,
                           signer.pasCounterSigners.psSigner.AuthAttrs.rgAttr.rgValue.pbData,
                           signer.pasCounterSigners.psSigner.AuthAttrs.rgAttr.rgValue.cbData,
                           0,@ft,n) then begin
        Result:=FileTimeToDatetime(ft,True);
      end;
    end;
    inc(signer.pasCounterSigners.psSigner.AuthAttrs.rgAttr);
  end;
end;

procedure ViewSignerInfo(AHandle,AStateData: THandle);
var
  viewSignerInfo: CRYPTUI_VIEWSIGNERINFO_STRUCT;
  provData: PCRYPT_PROVIDER_DATA;
  sgnr: PCRYPT_PROVIDER_SGNR;
begin
  provData:=WTHelperProvDataFromStateData(AStateData);
  if not Assigned(provData) then
    Exit;
  sgnr:=WTHelperGetProvSignerFromChain(provData,0,FALSE,0);
  if not Assigned(sgnr) then
    Exit;

  FillChar(ViewSignerInfo,sizeof(ViewSignerInfo),0);
  viewSignerInfo.dwSize:=sizeof(viewSignerInfo);
  viewSignerInfo.hwndParent:=AHandle;
  viewSignerInfo.pSignerInfo:=sgnr^.psSigner;
  viewSignerInfo.hMsg:=provData^.hMsg;
  viewSignerInfo.pszOID:=szOID_PKIX_KP_CODE_SIGNING;
  cryptUIDlgViewSignerInfo(@viewSignerInfo);
end;

function VerifyFile(const AFilename: string; var ASigner: string; var ATimestamp: TDatetime; AHandle: THandle = INVALID_HANDLE_VALUE): Integer;
var
  FileData: TWintrustFileInfo;
  WinTrustData: TWinTrustData;
  hAdmin,hFile,hCtx: THandle;
  {cb: Cardinal;
  buf: TBytes;}
  CatalogInfo: TCatalogInfo;
  WTDCatalogInfo: TWintrustCatalogInfo;
  s: string;
begin
  ASigner:='';
  ATimestamp:=0;
  hAdmin:=0;
  hCtx:=0;
  hFile:=INVALID_HANDLE_VALUE;
  try
    hFile:=CreateFile(PChar(AFileName),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
    if hFile<>INVALID_HANDLE_VALUE then begin
      {if CryptCATAdminAcquireContext(hAdmin,nil,0) then begin
        SetLength(buf,255);
        cb:=Length(buf);
        if CryptCATAdminCalcHashFromFileHandle(hFile,cb,@buf[0],0) then begin
          SetLength(buf,cb);
          s:=BytesToHEX(buf);
          hCtx:=CryptCATAdminEnumCatalogFromHash(hAdmin,@buf[0],cb,0,nil);
        end;
      end;}
    end;

    FillChar(FileData,SizeOf(TWintrustFileInfo),0);
    FillChar(WinTrustData,SizeOf(TWinTrustData),0);

    WinTrustData.cbStruct:=sizeof(TWinTrustData);
    WinTrustData.dwUIChoice:=WTD_UI_NONE;
    WinTrustData.fdwRevocationChecks:=WTD_REVOKE_WHOLECHAIN;
    WinTrustData.dwProvFlags:=WTD_SAFER_FLAG;
    WinTrustData.dwStateAction:=WTD_STATEACTION_VERIFY;
    if hCtx=0 then begin
      FileData.cbStruct:=sizeof(TWintrustFileInfo);
      FileData.pcwszFilePath:=PWideChar(AFilename);
      FileData.hFile:=hFile;
      WinTrustData.dwUnionChoice:=WTD_CHOICE_FILE;
      WinTrustData.InfoUnion.pFile:=@FileData;
    end else begin
      CryptCATCatalogInfoFromContext(hCtx,@CatalogInfo,0);
      FillChar(WTDCatalogInfo,SizeOf(WTDCatalogInfo),0);
      WTDCatalogInfo.cbStruct:=SizeOf(WTDCatalogInfo);
      WTDCatalogInfo.pcwszCatalogFilePath:=@CatalogInfo.wszCatalogFile[0];
      WTDCatalogInfo.pcwszMemberFilePath:=PWideChar(AFilename);
      WTDCatalogInfo.pcwszMemberTag:=PWideChar(s);
      WinTrustData.dwUnionChoice:=WTD_CHOICE_CATALOG;
      WinTrustData.InfoUnion.pCatalog:=@WTDCatalogInfo;
    end;

    Result:=WinVerifyTrustEx(INVALID_HANDLE_VALUE,WINTRUST_ACTION_GENERIC_VERIFY_V2,@WinTrustData);

    if (WinTrustData.hWVTStateData>0) then begin
      ASigner:=GetSigner(WinTrustData.hWVTStateData);
      ATimestamp:=GetTimestamp(WinTrustData.hWVTStateData);
    end;

    if AHandle<>INVALID_HANDLE_VALUE then
      ViewSignerInfo(AHandle,WinTrustData.hWVTStateData);

    WinTrustData.dwStateAction:=WTD_STATEACTION_CLOSE;

    WinVerifyTrustEx(INVALID_HANDLE_VALUE,WINTRUST_ACTION_GENERIC_VERIFY_V2,@WinTrustData);

  finally
    if hFile<>INVALID_HANDLE_VALUE then
      CloseHandle(hFile);
    if hCtx>0 then
      CryptCATAdminReleaseCatalogContext(hAdmin,hCtx,0);
    if hAdmin<>0 then
      CryptCATAdminReleaseContext(hAdmin,0);
  end;
end;

function VerifyFile(const AFilename: string; AHandle: THandle = INVALID_HANDLE_VALUE): Integer;
var
  s: string;
  dt: TDatetime;
begin
  Result:=VerifyFile(AFilename,s,dt,AHandle);
end;

procedure ViewPFXCertificate(AHandle: THandle; const AFilename, APassword: string);
var
  cert: PCCERT_CONTEXT;
  vcs: CRYPTUI_VIEWCERTIFICATE_STRUCT;
  b: Bool;
begin
  if GetPFXCertContext(AFilename,APassword,cert) then begin
    FillChar(vcs,SizeOf(vcs),0);
    vcs.dwSize:=SizeOf(vcs);
    vcs.hwndParent:=AHandle;
    vcs.pCertContext:=cert;
    CryptUIDlgViewCertificate(@vcs,b);
    CertFreeCertificateContext(cert);
  end;
end;

procedure ViewCertificate(AHandle: THandle; const AFilename: string);
var
  cert: PCCERT_CONTEXT;
  vcs: CRYPTUI_VIEWCERTIFICATE_STRUCT;
  b: Bool;
begin
  if GetCertContext(AFilename,cert) then begin
    FillChar(vcs,SizeOf(vcs),0);
    vcs.dwSize:=SizeOf(vcs);
    vcs.hwndParent:=AHandle;
    vcs.pCertContext:=cert;
    CryptUIDlgViewCertificate(@vcs,b);
    CertFreeCertificateContext(cert);
  end;
end;

function GetCertErrorText(ACode: integer): string;
begin
  case ACode of
    TRUST_E_PROVIDER_UNKNOWN: Result:='The trust provider is not recognized on this system';
    TRUST_E_ACTION_UNKNOWN: Result:='The trust verification action specified is not supported by the specified trust provider';
    TRUST_E_SUBJECT_FORM_UNKNOWN: Result:='The form specified for the subject is not one supported or known by the specified trust provider';
    TRUST_E_SUBJECT_NOT_TRUSTED: Result:='The subject is not trusted for the specified action';
    DIGSIG_E_ENCODE: Result:='Error due to problem in ASN.1 encoding process';
    DIGSIG_E_DECODE: Result:='Error due to problem in ASN.1 decoding process';
    DIGSIG_E_EXTENSIBILITY: Result:='Reading / writing Extensions where Attributes are appropriate, and visa versa';
    DIGSIG_E_CRYPTO: Result:='Unspecified cryptographic failure';
    PERSIST_E_SIZEDEFINITE: Result:='The size of the data could not be determined';
    PERSIST_E_SIZEINDEFINITE: Result:='The size of the indefinite-sized data could not be determined';
    PERSIST_E_NOTSELFSIZING: Result:='This object does not read and write self-sizing data';
    TRUST_E_NOSIGNATURE: Result:='No signature was present in the subject';
    CERT_E_EXPIRED: Result:='A required certificate is not within its validity period';
    $800B0102{CERT_E_VALIDITYPERIODNESTING}: Result:='The validity periods of the certification chain do not nest correctly';
    CERT_E_ROLE: Result:='A certificate that can only be used as an end-entity is being used as a CA or visa versa';
    CERT_E_PATHLENCONST: Result:='A path length constraint in the certification chain has been violated';
    CERT_E_CRITICAL: Result:='An extension of unknown type that is labeled ''critical'' is present in a certificate';
    CERT_E_PURPOSE: Result:='A certificate is being used for a purpose other than that for which it is permitted';
    CERT_E_ISSUERCHAINING: Result:='A parent of a given certificate in fact did not issue that child certificate';
    CERT_E_MALFORMED: Result:='A certificate is missing or has an empty value for an important field, such as a subject or issuer name';
    CERT_E_UNTRUSTEDROOT: Result:='A certification chain processed correctly, but terminated in a root certificate which isn''t trusted by the trust provider';
    CERT_E_CHAINING: Result:='A chain of certs didn''t chain as they should in a certain application of chaining';
    else Result:=Format('Error %8.8x',[ACode]);
  end;
end;

end.
