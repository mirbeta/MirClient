{***************************************************************************}
{ TvCard component                                                          }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit vCard;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, SysUtils, vCardBase64, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 7; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : Handling of photo type & encoding handling
  // v1.1.0.0 : New: Added support for Social profile iCloud information
  // v1.1.0.1 : Fixed : Issue with photo URI decoding over multiple lines
  // v1.1.0.2 : Improved : Compatibility with iCloud
  // v1.1.0.3 : Improved : vCalendar standards compliance
  // v1.1.0.4 : Fixed : Compatibility with non Unicode Delphi versions
  // v1.1.0.5 : Fixed : Memory leak when saving vCard without picture
  // v1.1.0.6 : Fixed : Issue with non UTF8 encoded files
  // v1.1.0.7 : Fixed : Issue with attributes on name value in VCF file

type
  TvCard = class;
  TvContact = class;

  TvCardVersion = (vvNone, vv21, vv30, vv40);

  TvPhoneType = (ptText, ptVoice, ptFax, ptCell, ptVideo, ptPager, ptTextPhone);
  TvFieldType = (ftHome, ftWork, ftOther);
  TvFileEncoding = (feExternalURL, feInternalBase64);
  TvImageType = (itGIF, itJPEG, itPNG);

  TvPhoneTypes = set of TvPhoneType;

  TvAddress = class(TCollectionItem)
  private
    FStreet: string;
    FCountry: string;
    FNumber: string;
    FPostCode: string;
    FCity: string;
    FPOBox: string;
    FRegion: string;
    FAddressType: TvFieldType;
    FMailingLabel: TStringList;
    FPreferred: boolean;
    procedure SetMailingLabel(const Value: TStringList);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property POBox: string read FPOBox write FPOBox;
    property Number: string read FNumber write FNumber;
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    property Region: string read FRegion write FRegion;
    property PostCode: string read FPostCode write FPostCode;
    property Country: string read FCountry write FCountry;
    property AddressType: TvFieldType read FAddressType write FAddressType;
    property MailingLabel: TStringList read FMailingLabel write SetMailingLabel;
    property Preferred: boolean read FPreferred write FPreferred;
  end;

  TvAddressCollection = class(TCollection)
  private
    FOwner: TvContact;
    function GetItem(Index: Integer): TvAddress;
    procedure SetItem(Index: Integer; const Value: TvAddress);

  public
    function ToXml: String;
    procedure LoadFromXml(XmlData: String);
    constructor Create(AOwner: TvContact);
    function Add: TvAddress;
    function Insert(Index: Integer): TvAddress;
    property Items[Index: Integer]: TvAddress read GetItem
      write SetItem; default;
    function GetOwner: TPersistent; override;
  end;

  TvPhone = class(TCollectionItem)
  private
    FPhoneNumber: string;
    FPhoneType: TvPhoneTypes;
    FPreferred: boolean;
    FFieldType: TvFieldType;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property PhoneNumber: string read FPhoneNumber write FPhoneNumber;
    property PhoneType: TvPhoneTypes read FPhoneType write FPhoneType;
    property FieldType: TvFieldType read FFieldType write FFieldType;
    property Preferred: boolean read FPreferred write FPreferred;
  end;

  TvPhoneCollection = class(TCollection)
  private
    FOwner: TvContact;
    function GetItem(Index: Integer): TvPhone;
    procedure SetItem(Index: Integer; const Value: TvPhone);

  public
    function ToXml: String;
    procedure LoadFromXml(XmlData: String);
    constructor Create(AOwner: TvContact);
    function Add: TvPhone;
    function Insert(Index: Integer): TvPhone;
    property Items[Index: Integer]: TvPhone read GetItem write SetItem; default;
    function GetOwner: TPersistent; override;
  end;

  TvEmail = class(TCollectionItem)
  private
    FEmailAddress: string;
    FEmailType: TvFieldType;
    FPreferred: boolean;
    procedure SetEmailAddress(const Value: string);
    procedure SetEmailType(const Value: TvFieldType);
    procedure SetPreferred(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property EmailAddress: string read FEmailAddress write SetEmailAddress;
    property EmailType: TvFieldType read FEmailType write SetEmailType;
    property Preferred: boolean read FPreferred write SetPreferred;
  end;

  TvEmailCollection = class(TCollection)
  private
    FOwner: TvContact;
    function GetEmail(Index: Integer): TvEmail;
    procedure SetEmail(Index: Integer; const Value: TvEmail);

  public
    function ToXml: String;
    procedure LoadFromXml(XmlData: String);
    constructor Create(AOwner: TvContact);
    function Add: TvEmail;
    function Insert(Index: Integer): TvEmail;
    property Items[Index: Integer]: TvEmail read GetEmail write SetEmail; default;
    function GetOwner: TPersistent; override;
  end;

  TvGeoLocation = class(TPersistent)
  private
    FLatitude: double;
    FLongitude: double;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Latitude: double read FLatitude write FLatitude;
    property Longitude: double read FLongitude write FLongitude;
  end;

  TvSocialItem = class(TCollectionItem)
  private
    FService: string;
    FURI: string;
    FUser: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property URI: string read FURI write FURI;
    property Service: string read FService write FService;
    property User: string read FUser write FUser;
  end;

  TvSocialProfile = class(TCollection)
  private
    FOwner: TvContact;
    function GetItem(Index: Integer): TvSocialItem;
    procedure SetItem(Index: Integer; const Value: TvSocialItem);
  public
    constructor Create(AOwner: TvContact);
    function Add: TvSocialItem;
    function Insert(Index: Integer): TvSocialItem;
    property Items[Index: Integer]: TvSocialItem read GetItem write SetItem; default;
    function GetOwner: TPersistent; override;
  end;

  TvContact = class(TCollectionItem)
  private
    FProfession: string;
    FBirthDay: TDateTime;
    FCompany: string;
    FNickName: string;
    FFullName: string;
    FJobTitle: string;
    FEmails: TvEmailCollection;
    FPhoneNumbers: TvPhoneCollection;
    FWebsiteURL: string;
    FAddresses: TvAddressCollection;
    FLastName: string;
    FFirstName: string;
    FNameSuffix: string;
    FMiddleName: string;
    FNamePrefix: string;
    FvCardVersion: TvCardVersion;
    FCategories: TStringList;
    FGeoLocation: TvGeoLocation;
    FSocialProfile: TvSocialProfile;
    FNote: TStringList;
    FUpdated: TDateTime;
    FSource: string;
    FID: string;
    FPhotoURL: string;
    FPhotoStr: string;
    FSortString: string;
    FTimeZone: string;
    FPhotoType: TvImageType;
    FPhotoEncoding: TvFileEncoding;
    FPhoto: TPicture;
    FProdID: string;
    procedure SetBirthDay(const Value: TDateTime);
    procedure SetCompany(const Value: string);
    procedure SetFullName(const Value: string);
    procedure SetJobTitle(const Value: string);
    procedure SetNickName(const Value: string);
    procedure SetFProfession(const Value: string);
    procedure SetWebsiteURL(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetMiddleName(const Value: string);
    procedure SetNamePrefix(const Value: string);
    procedure SetNameSuffix(const Value: string);
    procedure SetvCardVersion(const Value: TvCardVersion);
    procedure SetCategories(const Value: TStringList);
    procedure SetNote(const Value: TStringList);
    procedure SetUpdated(const Value: TDateTime);
    procedure SetSource(const Value: string);
    procedure SetID(const Value: string);
    procedure SetPhotoURL(const Value: string);
    procedure SetSortString(const Value: string);
    procedure SetTimeZone(const Value: string);
    procedure SetPhotoEncoding(const Value: TvFileEncoding);
    procedure SetPhotoType(const Value: TvImageType);
    procedure SetPhoto(const Value: TPicture);
    procedure SetAddresses(const Value: TvAddressCollection);
    procedure SetEmails(const Value: TvEmailCollection);
    procedure SetPhoneNumbers(const Value: TvPhoneCollection);
    procedure SetSocialProfile(const Value: TvSocialProfile);
  protected
    property PhotoStr: string read FPhotoStr write FPhotoStr;
    procedure DecodePhoto;
    procedure EncodePhoto;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property ID: string read FID write SetID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property MiddleName: string read FMiddleName write SetMiddleName;
    property NamePrefix: string read FNamePrefix write SetNamePrefix;
    property NameSuffix: string read FNameSuffix write SetNameSuffix;
    property FullName: string read FFullName write SetFullName;
    property NickName: string read FNickName write SetNickName;
    property Company: string read FCompany write SetCompany;
    property JobTitle: string read FJobTitle write SetJobTitle;
    property BirthDay: TDateTime read FBirthDay write SetBirthDay;
    property Profession: string read FProfession write SetFProfession;
    property WebsiteURL: string read FWebsiteURL write SetWebsiteURL;
    property Emails: TvEmailCollection read FEmails write SetEmails;
    property PhoneNumbers: TvPhoneCollection read FPhoneNumbers write SetPhoneNumbers;
    property Addresses: TvAddressCollection read FAddresses write SetAddresses;
    property vCardVersion: TvCardVersion read FvCardVersion write SetvCardVersion default vv30;
    property Categories: TStringList read FCategories write SetCategories;
    property GeoLocation: TvGeoLocation read FGeoLocation write FGeoLocation;
    property Note: TStringList read FNote write SetNote;
    property Updated: TDateTime read FUpdated write SetUpdated;
    property Source: string read FSource write SetSource;
    property Photo: TPicture read FPhoto write SetPhoto;
    property PhotoURL: string read FPhotoURL write SetPhotoURL;
    property PhotoEncoding: TvFileEncoding read FPhotoEncoding write SetPhotoEncoding default feInternalBase64;
    property PhotoType: TvImageType read FPhotoType write SetPhotoType default itJPEG;
    property ProdID: string read FProdID write FProdID;
    property SocialProfile: TvSocialProfile read FSocialProfile write SetSocialProfile;
    property SortString: string read FSortString write SetSortString;
    property TimeZone: string read FTimeZone write SetTimeZone;
  end;

  TvContactsCollection = class(TCollection)
  private
    FOwner: TvCard;
    function GetvContact(Index: Integer): TvContact;
    procedure SetvContact(Index: Integer; const Value: TvContact);

  public
    constructor Create(AOwner: TvCard);
    function Add: TvContact;
    function Insert(Index: Integer): TvContact;
    property Items[Index: Integer]: TvContact read GetvContact write SetvContact; default;
    function GetOwner: TPersistent; override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TvCard = class(TComponent)
  private
    FvContacts: TvContactsCollection;
    FProdID: string;
    function GetvContacts: TvContactsCollection;
    procedure SetvContacts(const Value: TvContactsCollection);
    function GeneratevCard(vContact: TvContact): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure InsertFromStream(Stream: TStream);
    function SaveToString: string;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure InsertFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string; vContact: TvContact); overload;
    procedure SaveToFile(const FileName: string); overload;
  published
    property vContacts: TvContactsCollection read GetvContacts write SetvContacts;
    property ProdID: string read FProdID write FProdID;
  end;


  TFileStringList = class(TStringList)
  private
    fp: integer;
    cache: string;
    function GetEOF: boolean;
  public
    procedure Reset;
    procedure ReadLn(var s: string);
    procedure Write(s: string);
    procedure WriteLn(s: string);
    property Eof: boolean read GetEOF;
  end;

implementation

uses
  JPEG, Dialogs, XmlIntf, XmlDoc
  {$IFDEF DELPHIXE_LVL}
  , GIFImg, PNGImage
  {$ENDIF}
  {$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
  {$ENDIF}
  ;

const
  lf = #10;
  crlf = #13#10;

  {$IFNDEF DELPHI_UNICODE}
type
  TBytes = Array of Byte;
  {$ENDIF}



procedure TFileStringList.Reset;
begin
  fp := 0;
  cache := '';
end;

function TFileStringList.GetEOF;
begin
  Result := fp >= Count;
end;

procedure TFileStringList.ReadLn(var s: string);
begin
  s := Strings[fp];
  inc(fp);
end;

procedure TFileStringList.Write(s: string);
begin
  cache := cache + s;
end;

procedure TFileStringList.WriteLn(s: string);
begin
  Add(cache + s);
  cache := '';
end;

{$I DELPHIXE.INC}

{ TvCard }

constructor TvCard.Create(AOwner: TComponent);
begin
  inherited;
  FvContacts := TvContactsCollection.Create(Self);
end;

destructor TvCard.Destroy;
begin
  FvContacts.Free;
  inherited;
end;

function TvCard.GetvContacts: TvContactsCollection;
begin
  Result := FvContacts;
end;

procedure TvCard.LoadFromFile(const FileName: string);
begin
  vContacts.Clear;
  InsertFromFile(FileName);
end;

procedure TvCard.LoadFromStream(Stream: TStream);
begin
  vContacts.Clear;
  InsertFromStream(Stream);
end;

function ConvertFromCRLF(s: string): string;
begin
  s := StringReplace(s, '=0D', #13, [rfReplaceAll]);
  s := StringReplace(s, '=0A', #10, [rfReplaceAll]);
  s := StringReplace(s, '=3D', '=', [rfReplaceAll]);
  Result := s;
end;

function ConvertToCRLF(s: string): string;
begin
  s := StringReplace(s, '=', '=3D', [rfReplaceAll]);
  s := StringReplace(s, #13, '=0D', [rfReplaceAll]);
  s := StringReplace(s, #10, '=0A', [rfReplaceAll]);
  Result := s;
end;

function DelSpaces(s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] <> ' ' then
      Result := Result + s[i];
end;

function PreferredToStr(Value: boolean; vv: TvCardVersion): string;
begin
  Result := '';
  if Value then
  begin
    if vv = vv21 then
      Result := ';PREF'
    else
      Result := ';TYPE=PREF';
  end;
end;

function StrToPreferred(s: string): boolean;
begin
  s := UpperCase(s);
  if Pos('PREF', s) > 0 then
    Result := true
  else
    Result := false;
end;

function FieldTypeToStr(ft: TvFieldType; vv: TvCardVersion): string;
begin
  Result := '';
  if ft = ftWork then
    Result := 'WORK'
  else if ft = ftHome then
    Result := 'HOME';
  if vv = vvNone then
  begin
    if Result = '' then
      Result := 'OTHER';
    Exit;
  end;
  if vv = vv21 then
    Result := ';' + Result
  else
    Result := ';TYPE=' + Result;
end;

function StrToFieldType(s: string): TvFieldType;
begin
  s := UpperCase(s);
  if Pos('HOME', s) > 0 then
    Result := ftHome
  else if Pos('WORK', s) > 0 then
    Result := ftWork
  else
    Result := ftOther;
end;

function StrToPhoneType(s: string): TvPhoneTypes;
begin
  Result := [];
  s := UpperCase(s);

  if Pos('TEXT', s) > 0 then
    Result := Result + [ptText];

  if Pos('VOICE', s) > 0 then
    Result := Result + [ptVoice];

  if Pos('FAX', s) > 0 then
    Result := Result + [ptFax];

  if Pos('CELL', s) > 0 then
    Result := Result + [ptCell];

  if Pos('VIDEO', s) > 0 then
    Result := Result + [ptVideo];

  if Pos('PAGER', s) > 0 then
    Result := Result + [ptPager];

  if Pos('TEXTPHONE', s) > 0 then
    Result := Result + [ptTextPhone];
end;

function GetProps(s: string): string;
begin
  Result := Copy(s, Pos(';', s) + 1, Pos(':', s) - Pos(';', s) - 1);
end;

function GetPropVal(s: string): string;
var
  pc,pq: integer;
begin
  Result := '';

  if Pos(':', s) > 0 then
    Delete(s, 1, Pos(':', s));

  pc := Pos(';',s);
  pq := Pos('"',s);

  if (pc > pq) or ((pc > 0) and (pq = 0)) then
    s := Copy(s, 1, pc - 1);

  if (pq > 0) then
  begin
    Delete(s, 1, pq);
    pq := Pos('"', s);
    if pq > 0 then
      Delete(s, 1, pq);
  end;

  s := StringReplace(s,'\,',',',[rfReplaceAll]);

  Result := Trim(s);
end;

function IsoToDate(s: string): TDateTime;
var
  da, mo, ye: Word;
  err: Integer;
begin
  Val(Copy(s, 1, 4), ye, err);
  Val(Copy(s, 5, 2), mo, err);
  Val(Copy(s, 7, 2), da, err);
  Result := EncodeDate(ye, mo, da);
end;

function IsoToDateTime(s: string): TDateTime;
var
  da, mo, ye, ho, mi, se: Word;
  err: Integer;
begin
  if Pos('-', s) > 0 then
  begin
    Val(Copy(s, 1, 4), ye, err);
    Val(Copy(s, 6, 2), mo, err);
    Val(Copy(s, 9, 2), da, err);
    Val(Copy(s, 10, 2), ho, err);
    Val(Copy(s, 12, 2), mi, err);
    Val(Copy(s, 14, 2), se, err);
  end
  else
  begin
    Val(Copy(s, 1, 4), ye, err);
    Val(Copy(s, 5, 2), mo, err);
    Val(Copy(s, 7, 2), da, err);
    Val(Copy(s, 10, 2), ho, err);
    Val(Copy(s, 12, 2), mi, err);
    Val(Copy(s, 14, 2), se, err);
  end;

  Result := EncodeDate(ye, mo, da) + EncodeTime(ho, mi, se, 0);
end;

function IntToZStr(i, l: Integer): string;
var
  Res: string;
begin
  Res := IntToStr(i);
  while Length(Res) < l do
    Res := '0' + Res;

  Result := Res;
end;

function DateTimeToIso(dt: TDateTime): string;
var
  da, mo, ye, ho, mi, se, ms: Word;
begin
  DecodeDate(dt, ye, mo, da);
  DecodeTime(dt, ho, mi, se, ms);
  Result := IntToStr(ye) + '-' + IntToZStr(mo, 2) + '-' + IntToZStr(da, 2) + 'T'
    + IntToZStr(ho, 2) + ':' + IntToZStr(mi, 2) + ':' + IntToZStr(se, 2) + 'Z';
end;

procedure TvCard.InsertFromFile(const FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    InsertFromStream(fs);
  finally
    fs.Free;
  end;
end;

function VarPos(sub, s: string; var vp: Integer): Integer;
begin
  vp := Pos(sub, s);
  Result := vp;
end;

procedure TvCard.InsertFromStream(Stream: TStream);
var
  s, str, props, splitchar, LastProp, su: string;
  vContact: TvContact;
  em: TvEmail;
  tel: TvPhone;
  adr: TvAddress;
  i, j, vp: Integer;
  sl: TFileStringList;
  si: TvSocialItem;

  function NCPos(search, s: string): integer;
  begin
    Result := Pos(search, Uppercase(s));
  end;

begin
  vContact := nil;
  adr := nil;

  sl := TFileStringList.Create;
  {$IFDEF DELPHI_UNICODE}
  sl.LoadFromStream(Stream, TEncoding.UTF8);
  if sl.Count = 0 then
  begin
    Stream.Position := 0;
    sl.LoadFromStream(Stream);
  end;

  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  sl.LoadFromStream(Stream);
  {$ENDIF}

  try
    j := vContacts.Count;

    while not sl.Eof do
    begin
      sl.ReadLn(s);

      s := ConvertFromCRLF(s);

      if NCPos('BEGIN:VCARD', DelSpaces(s)) = 1 then
      begin
        vContact := vContacts.Add;
      end;

      if NCPos('END:VCARD', DelSpaces(s)) = 1 then
      begin
        vContact := Nil;
        LastProp := '';
      end;

      if (NCPos('VERSION', s) = 1) and Assigned(vContact) then
      begin
        if Pos('3.0', s) > 0 then
          vContact.vCardVersion := vv30
        else if Pos('4.0', s) > 0 then
          vContact.vCardVersion := vv40
        else
          vContact.vCardVersion := vv21;
        LastProp := '';
      end;

      if (NCPos('UID:', s) = 1) and Assigned(vContact) then
      begin
        Delete(s, 1, 4);
        while Pos(':', s) > 0 do
        begin
          Delete(s, 1, Pos(':', s));
        end;
        vContact.ID := s;
        LastProp := '';
      end;

      if (NCPos('PRODID:', s) = 1) and Assigned(vContact) then
      begin
        Delete(s, 1, 7);
        vContact.ProdID := s;
      end;

      if (NCPos('N;', s) = 1) then  // N tag has an attribute
      begin
        Delete(s, 1, 2);
        vp := Pos(':', s);  // skip attribute
        if vp > 0 then
          Delete(s,1,vp);

        vp := Pos(';', s);
        if vp > 0 then
        begin
          vContact.LastName := Copy(s, 1, vp - 1);
          Delete(s, 1, vp);

          vp := Pos(';', s);
          if vp > 0 then
          begin
            vContact.MiddleName := Copy(s, 1, vp - 1);
            Delete(s, 1, vp);
            vp := Pos(';', s);
            if vp > 0 then
            begin
              vContact.FirstName := Copy(s, 1, vp - 1);
            end;
          end
          else
          begin
            vContact.FirstName := s;
          end;
        end;
      end;

      if (NCPos('N:', s) = 1) then
      begin
        Delete(s, 1, 2);
        vp := Pos(';', s);
        if vp > 0 then
        begin
          vContact.LastName := Copy(s, 1, vp - 1);
          Delete(s, 1, vp);

          vp := Pos(';', s);
          if vp > 0 then
          begin
            vContact.MiddleName := Copy(s, 1, vp - 1);
            Delete(s, 1, vp);
            vp := Pos(';', s);
            if vp > 0 then
            begin
              vContact.FirstName := Copy(s, 1, vp - 1);
            end;
          end
          else
            vContact.FirstName := s;
        end;
      end;

      if (NCPos('FN:', s) = 1) and Assigned(vContact) then
      begin
        vContact.FullName := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('NICKNAME', s) = 1) and Assigned(vContact) then
      begin
        vContact.NickName := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('ORG', s) = 1) and Assigned(vContact) then
      begin
        vContact.Company := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('TITLE', s) = 1) and Assigned(vContact) then
      begin
        vContact.JobTitle := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('ROLE', s) = 1) and Assigned(vContact) then
      begin
        vContact.Profession := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('SOURCE', s) = 1) and Assigned(vContact) then
      begin
        vContact.Source := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('SORT-STRING', s) = 1) and Assigned(vContact) then
      begin
        vContact.SortString := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('TIMEZONE', s) = 1) and Assigned(vContact) then
      begin
        vContact.TimeZone := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('BDAY', s) = 1) and Assigned(vContact) then
      begin
        vContact.BirthDay := IsoToDate(GetPropVal(s));
        LastProp := '';
      end;

      if (NCPos('CATEGORIES', s) = 1) and Assigned(vContact) then
      begin
        vContact.Categories.CommaText := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('GEO', s) = 1) and Assigned(vContact) then
      begin
        s := GetPropVal(s);
        i := 0;
        if vContact.vCardVersion = vv40 then
          splitchar := ','
        else
          splitchar := ';';

        while Pos(splitchar, s) > 0 do
        begin
          str := (Copy(s, 1, Pos(splitchar, s) - 1));

          if i = 0 then
            vContact.GeoLocation.Latitude := StrToFloat(StringReplace(str, '.', DecimalSeparator, []));
          if i = 1 then
            vContact.GeoLocation.Longitude := StrToFloat(StringReplace(str, '.', DecimalSeparator, []));

          Delete(s, 1, Pos(splitchar, s));
          i := i + 1;
        end;
        LastProp := '';
      end;

      if (NCPos('NOTE', s) = 1) and Assigned(vContact) then
      begin
        vContact.Note.Text := StringReplace(GetPropVal(s), '\n', #13, [rfReplaceAll]);
        LastProp := '';
      end;

      if (NCPos('REV', s) = 1) and Assigned(vContact) then
      begin
        vContact.Updated := IsoToDateTime(GetPropVal(s));
        LastProp := '';
      end;

      if (NCPos('EMAIL', s) = 1) and Assigned(vContact) then
      begin
        em := vContact.Emails.Add;
        props := GetProps(s);
        em.Preferred := StrToPreferred(props);
        em.EmailType := StrToFieldType(props);
        em.EmailAddress := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('TEL', s) = 1) and Assigned(vContact) then
      begin
        tel := vContact.PhoneNumbers.Add;
        tel.PhoneNumber := GetPropVal(s);
        props := GetProps(s);
        tel.Preferred := StrToPreferred(props);
        tel.PhoneType := StrToPhoneType(props);
        tel.FieldType := StrToFieldType(props);
        LastProp := '';
      end;

      if ((NCPos('ADR', s) = 1) or (NCPos('.ADR;',s) > 0)) and Assigned(vContact) then
      begin
        adr := vContact.Addresses.Add;
        props := GetProps(s);
        adr.Preferred := StrToPreferred(props);
        adr.AddressType := StrToFieldType(props);

        if (NCPos('LABEL', props) > 0) and (vContact.vCardVersion = vv40) then
        begin
          props := StringReplace(props, '"', '', [rfReplaceAll]);
          props := Copy(props, Pos('LABEL', props) + 6, Length(props));
          adr.MailingLabel.Text := StringReplace(props, '\n', #13, [rfReplaceAll]);;
        end;

        Delete(s, 1, Pos(':', s));
        i := 0;
        while Pos(';', s) > 0 do
        begin
          str := Copy(s, 1, Pos(';', s) - 1);

          if i = 0 then
            adr.POBox := str;
          if i = 1 then
            adr.Number := str;
          if i = 2 then
            adr.Street := str;
          if i = 3 then
            adr.City := str;
          if i = 4 then
            adr.Region := str;
          if i = 5 then
            adr.PostCode := str;
          if i = 6 then
            adr.Country := str;

          Delete(s, 1, Pos(';', s));
          i := i + 1;
        end;

        if (s <> '') and (i = 6) then
          adr.Country := s;

        LastProp := '';
      end;


      if (NCPos('X-SOCIALPROFILE',s) = 1) and Assigned(vContact) then
      begin
        si := vContact.SocialProfile.Add;
        props := GetProps(s);

        if NCPos('TYPE=',props) > 0 then
        begin
          su := Copy(props,NCPos('TYPE=',props)+5,length(props));
          if pos(';',su) > 0 then
            si.Service := copy(su,1,pos(';',su)-1)
          else
            si.Service := su;
        end;

        if NCPos('X-USER=',props) > 0 then
        begin
          su := Copy(props, NCPos('X-USER=',props)+7,length(props));
          if pos(';',su) > 0 then
            si.User := copy(su,1,pos(';',su)-1)
          else
            si.User := su;
        end;

        Delete(s,1,Pos(':',s));
        si.URI := s;
        LastProp := '';
      end;

      if (NCPos('LABEL', s) = 1) and Assigned(vContact) and (adr <> nil) and
        (vContact.vCardVersion <> vv40) then
      begin
        adr.MailingLabel.Text := GetPropVal(s);
        adr := nil;
        LastProp := '';
      end;

      if (NCPos('URL', s) = 1) and Assigned(vContact) then
      begin
        vContact.WebsiteURL := GetPropVal(s);
        LastProp := '';
      end;

      if (NCPos('PHOTO', s) = 1) and Assigned(vContact) then
      begin
        props := UpperCase(GetProps(s));
        if NCPos('JPEG', props) > 0 then
          vContact.PhotoType := itJPEG
        else if NCPos('GIF', props) > 0 then
          vContact.PhotoType := itGIF
        else if NCPos('PNG', props) > 0 then
          vContact.PhotoType := itPNG;

        if (NCPos('LUE=URI:', s) > 0) then
        begin
          vContact.PhotoEncoding := feExternalURL;
          vContact.PhotoURL := Copy(s, NCPos('LUE=URI:', s) + 8, 65535);
          LastProp := 'PhotoURL';
        end
        else
        begin
          vContact.PhotoEncoding := feInternalBase64;
          vContact.PhotoStr :=  ''; // GetPropVal(s);
          LastProp := 'Photo';
        end;
      end;

      if (LastProp = 'PhotoURL') and Assigned(vContact) then
      begin
        if Pos(':', s) = 0 then
          vContact.PhotoURL := vContact.PhotoURL + Trim(s)
      end;

      if (LastProp = 'Photo') and Assigned(vContact) then
      begin
        if Pos(':', s) <> 0 then
          s := Copy(s, Pos(':', s) + 1, 65535);
        vContact.PhotoStr := vContact.PhotoStr + Trim(s);
      end;
    end;

    for i := j to vContacts.Count - 1 do
    begin
      vContacts[i].DecodePhoto;
    end;
  finally
    sl.Free;
  end;
end;

function QuotedPrintableEncode(toencode: string): string;
var
  i, NbChar: Integer;
  NewEncodedLine: boolean;
  MyResult: string;

begin { QuotedPrintableEncode }
  MyResult := '';
  NewEncodedLine := false;
  NbChar := 0;
  for i := 1 to Length(toencode) do
  begin
    inc(NbChar, 1);
    case ord(toencode[i]) of
      10, 13:
        begin
          MyResult := MyResult + '=' + IntToHex(ord(toencode[i]), 2);
          NewEncodedLine := false;
          NbChar := 0;
        end;
      9, 32:
        if NewEncodedLine = true then
          MyResult := MyResult + '=' + IntToHex(ord(toencode[i]), 2)
        else
          MyResult := MyResult + toencode[i];
      33 .. 60, 62 .. 126:
        MyResult := MyResult + toencode[i];
    else
      MyResult := MyResult + '=' + IntToHex(ord(toencode[i]), 2);
    end;
    if NbChar > 76 then
    begin
      NbChar := 0;
      MyResult := MyResult + '=' + crlf;
      NewEncodedLine := true;
    end;
  end;
  Result := MyResult;
end; { QuotedPrintableEncode }

function TvCard.GeneratevCard(vContact: TvContact): string;
var
  i, j, istart, iend: Integer;
  Ty, Pt, Line, fn, ss: string;

  function Escape(const s: string): string;
  begin
    Result := StringReplace(s,',','\,',[rfReplaceAll]);
  end;

begin
  Result := 'BEGIN:VCARD' + lf;

  case vContact.vCardVersion of
    vv21:
      Result := Result + 'VERSION:2.1' + lf;
    vv30:
      Result := Result + 'VERSION:3.0' + lf;
    vv40:
      Result := Result + 'VERSION:4.0' + lf;
  end;

  Result := Result + 'N';
  if (vContact.SortString <> '') and (vContact.vCardVersion = vv40) then
    Result := Result + ';SORT-STRING=' + vContact.SortString;
  Result := Result + ':'
    + vContact.FirstName + ';'
    + vContact.LastName + ';'
    + vContact.MiddleName + ';'
    + vContact.NamePrefix + ';'
    + vContact.NameSuffix + lf;

  fn := vContact.FullName;
  if Trim(fn) = '' then
    fn := vContact.FirstName + ' ' + vContact.LastName;

  Result := Result + 'FN:' + Escape(fn) + lf;

  if vContact.ID <> '' then
    // result := result + 'UID:urn:uuid:' + vContact.ID + lf;
    Result := Result + 'UID:' + vContact.ID + lf;
  if vContact.NickName <> '' then
    Result := Result + 'NICKNAME:' + vContact.NickName + lf;
  if vContact.Company <> '' then
    Result := Result + 'ORG:' + Escape(vContact.Company) + lf;
  if vContact.JobTitle <> '' then
    Result := Result + 'TITLE:' + Escape(vContact.JobTitle) + lf;
  if vContact.Profession <> '' then
    Result := Result + 'ROLE:' + Escape(vContact.Profession) + lf;
  if vContact.WebsiteURL <> '' then
    Result := Result + 'URL;WORK:' + vContact.WebsiteURL + lf;
  if vContact.Source <> '' then
    Result := Result + 'SOURCE:' + Escape(vContact.Source) + lf;
  if vContact.TimeZone <> '' then
    Result := Result + 'TZ:' + vContact.TimeZone + lf;
  if vContact.BirthDay <> 0 then
    Result := Result + 'BDAY:' + FormatDateTime('YYYYmmDD',
      vContact.BirthDay) + lf;

  if vContact.Updated <> 0 then
    Result := Result + 'REV:' + DateTimeToIso(vContact.Updated) + lf
  else
    Result := Result + 'REV:' + DateTimeToIso(Now) + lf;

  if ProdID <> '' then
    Result := Result + 'PRODID:' + ProdID + lf;

  if vContact.Categories.Text <> '' then
  begin
    vContact.Categories.Delimiter := ',';
    Result := Result + 'CATEGORIES:' + vContact.Categories.CommaText + lf;
  end;

  if vContact.Note.Text <> '' then
  begin
    if vContact.vCardVersion <> vv21 then
    begin
      Result := Result + 'NOTE:';
      for i := 0 to vContact.Note.Count - 1 do
        Result := Result + vContact.Note[i] + '\n';
      Result := Result + lf;
    end
    else
      Result := Result + 'NOTE;ENCODING=QUOTED-PRINTABLE:' +
        ConvertToCRLF(vContact.Note.Text) + lf
  end;

  if (vContact.GeoLocation.Latitude <> 0) and
    (vContact.GeoLocation.Longitude <> 0) then
  begin
    Result := Result + 'GEO:';
    if vContact.vCardVersion = vv40 then
      Result := Result + 'geo:';

    Result := Result + StringReplace(FloatToStr(vContact.GeoLocation.Latitude),
      DecimalSeparator, '.', []);

    if vContact.vCardVersion = vv40 then
      Result := Result + ','
    else
      Result := Result + ';';

    Result := Result + StringReplace(FloatToStr(vContact.GeoLocation.Longitude),
      DecimalSeparator, '.', []);

    Result := Result + lf;
  end;


  ss := vContact.SortString;

  if ss = '' then
    ss := vContact.FullName;

  if (ss <> '') and (vContact.vCardVersion <> vv40) then
  begin
    Result := Result + 'SORT-STRING:' + ss + lf;
  end;

  for i := 0 to vContact.Addresses.Count - 1 do
  begin
    Result := Result + 'ADR';
    Result := Result + PreferredToStr(vContact.Addresses[i].Preferred,
      vContact.vCardVersion);
    Result := Result + FieldTypeToStr(vContact.Addresses[i].AddressType,
      vContact.vCardVersion);

    if vContact.Addresses[i].MailingLabel.Text <> '' then
    begin
      if vContact.vCardVersion = vv40 then
        Result := Result + ';LABEL="';
      for j := 0 to vContact.Addresses[i].MailingLabel.Count - 1 do
        Result := Result + vContact.Addresses[i].MailingLabel[j] + '\n';
    end;

    Result := Result + ':' + vContact.Addresses[i].FPOBox + ';' +
      vContact.Addresses[i].Number + ';' + vContact.Addresses[i].Street + ';' +
      vContact.Addresses[i].City + ';' + vContact.Addresses[i].Region + ';' +
      vContact.Addresses[i].PostCode + ';' + vContact.Addresses[i].Country + lf;

    if vContact.Addresses[i].MailingLabel.Text <> '' then
    begin
      if vContact.vCardVersion <> vv40 then
        Result := Result + 'LABEL;ENCODING=QUOTED-PRINTABLE:' +
          ConvertToCRLF(vContact.Addresses[i].MailingLabel.Text) + lf;
    end;
  end;

  for i := 0 to vContact.PhoneNumbers.Count - 1 do
  begin
    Ty := '';
    if ptText in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + 'TEXT';
    if ptVoice in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + ',VOICE';
    if ptFax in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + ',FAX';
    if ptCell in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + ',CELL';
    if ptVideo in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + ',VIDEO';
    if ptPager in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + ',PAGER';
    if ptTextPhone in vContact.PhoneNumbers[i].PhoneType then
      Ty := Ty + ',TEXTPHONE';

    if Pos(',', Ty) = 1 then
      Delete(Ty, 1, 1);

    if Ty <> '' then
    begin
      if vContact.vCardVersion <> vv21 then
        Ty := ';TYPE=' + Ty
      else
        Ty := ';' + Ty;
    end;

    Result := Result + 'TEL' + Ty + PreferredToStr
      (vContact.PhoneNumbers[i].Preferred, vContact.vCardVersion) + ':' +
      vContact.PhoneNumbers[i].PhoneNumber + lf;
  end;

  for i := 0 to vContact.Emails.Count - 1 do
  begin
    if vContact.vCardVersion = vv21 then
      Result := Result + 'EMAIL;INTERNET'
    else
      Result := Result + 'EMAIL;TYPE=INTERNET';

    Result := Result + PreferredToStr(vContact.Emails[i].Preferred,
      vContact.vCardVersion);
    Result := Result + FieldTypeToStr(vContact.Emails[i].EmailType,
      vContact.vCardVersion);
    Result := Result + ':' + vContact.Emails[i].EmailAddress + lf;
  end;

  for I := 0 to vContact.SocialProfile.Count - 1 do
  begin
    Result := Result + 'X-SOCIALPROFILE;type=' + vContact.SocialProfile[i].Service + ';x-user='+vContact.SocialProfile[i].User+':'+vContact.SocialProfile[i].URI + lf;
  end;

  vContact.EncodePhoto;
  if vContact.PhotoStr <> '' then
  begin

    if vContact.PhotoType = itGIF then
      Pt := 'GIF'
    else if vContact.PhotoType = itJPEG then
      Pt := 'JPEG'
    else if vContact.PhotoType = itPNG then
      Pt := 'PNG';

    if vContact.PhotoEncoding = feInternalBase64 then 
    begin
      case vContact.vCardVersion of
        // spec:
        // vv21: result := result + 'PHOTO;' + Pt + ';ENCODING=BASE64:';
        // outlook:
        // vv21: result := result + 'PHOTO;TYPE=' + Pt + ';ENCODING=BASE64:';
        // workaround: outlook does not import photos in v21 format
        vv21:
          begin
            Result := Result + 'PHOTO;ENCODING=b;TYPE=' + Pt + ':';
            //ls := Length('PHOTO;ENCODING=b;TYPE=' + Pt + ':');
          end;
        // spec:
        // vv30: result := result + 'PHOTO;TYPE=' + Pt + ';ENCODING=B:';
        // ios:
        vv30:
          begin
            Result := Result + 'PHOTO;ENCODING=BASE64;TYPE=' + Pt + ':';
            //ls := Length('PHOTO;ENCODING=BASE64;TYPE=' + Pt + ':');
          end;
        // spec:
        // vv40: result := result + 'PHOTO:data:image/' + LowerCase(Pt) + ';base64,';
        // outlook & ios:
        vv40:
          begin
            Result := Result + 'PHOTO;ENCODING=BASE64;TYPE=' + Pt + ':' + lf;
            //ls := Length('PHOTO;ENCODING=b;TYPE=' + Pt + ':');
          end;
      end;

      // istart := 1;
      // iend := 49;
      // for I := istart to iend - 1 do
      // begin
      // Result := Result + vContact.PhotoStr[I];
      // end;
      // Result := Result + lf;

      iend := 1;
      Result := Result + crlf;

      while iend < Length(vContact.PhotoStr) do
      begin
        istart := iend;
        iend := (istart + 74);
        //ls := 0;
        Line := ' ';
        for i := istart to iend - 1 do
        begin
          if i < Length(vContact.PhotoStr) then
            /// Result := Result + vContact.Photo[I];
            Line := Line + vContact.PhotoStr[i];
        end;
        // outputdebugstring(pchar('*'+line+'*'));
        Result := Result + Line + crlf;
        // Result := Result + lf;
      end;
    end
    else
    begin
      case vContact.vCardVersion of
        vv21:
          Result := Result + 'PHOTO;' + Pt + ':';
        vv30:
          Result := Result + 'PHOTO;TYPE=' + Pt + ':';
        vv40:
          Result := Result + 'PHOTO:MEDIATYPE=image/' + LowerCase(Pt) + ':';
      end;
      Result := Result + vContact.PhotoStr + lf;
    end;

  end;

  Result := Result + 'END:VCARD';
end;

procedure TvCard.SaveToFile(const FileName: string);
var
  fs: TFileStream;

begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TvCard.SaveToStream(Stream: TStream);
var
  sl: TFileStringList;
  i: integer;
  {$IFDEF DELPHI_UNICODE}
  s:AnsiString;
  {$ENDIF}
begin
  Stream.Size := 0;
  sl := TFileStringList.Create;
  try
    for i := 0 to vContacts.Count - 1 do
    begin
      sl.WriteLn(GeneratevCard(vContacts[i]));
    end;
    {$IFDEF DELPHI_UNICODE}
    s := UTF8Encode(sl.Text);
    Stream.Write(s[1],Length(s));
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    sl.SaveToStream(Stream);
    {$ENDIF}
  finally
    sl.Free;
  end;
end;

function TvCard.SaveToString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to vContacts.Count - 1 do
  begin
    Result := Result + GeneratevCard(vContacts[i]) + lf;
  end;
end;

procedure TvCard.SaveToFile(const FileName: string; vContact: TvContact);
var
  tf: Text;
begin
  AssignFile(tf, FileName);
{$I-}
  Rewrite(tf);
{$I+}
  if IOResult = 0 then
  begin
    WriteLn(tf, GeneratevCard(vContact));
    CloseFile(tf);
  end;
end;

procedure TvCard.SetvContacts(const Value: TvContactsCollection);
begin
  FvContacts.Assign(Value);
end;

{ TvContact }

procedure TvContact.Assign(Source: TPersistent);
begin
  if (Source is TvContact) then
  begin
    FID := (Source as TvContact).ID;
    FFirstName := (Source as TvContact).FirstName;
    FLastName := (Source as TvContact).LastName;
    FMiddleName := (Source as TvContact).MiddleName;
    FNamePrefix := (Source as TvContact).NamePrefix;
    FNameSuffix := (Source as TvContact).NameSuffix;
    FFullName := (Source as TvContact).FullName;
    FNickName := (Source as TvContact).NickName;
    FCompany := (Source as TvContact).Company;
    FJobTitle := (Source as TvContact).JobTitle;
    FBirthDay := (Source as TvContact).BirthDay;
    FProfession := (Source as TvContact).Profession;
    FWebsiteURL := (Source as TvContact).WebsiteURL;
    FEmails.Assign((Source as TvContact).Emails);
    FPhoneNumbers.Assign((Source as TvContact).PhoneNumbers);
    FAddresses.Assign((Source as TvContact).Addresses);
    FvCardVersion := (Source as TvContact).vCardVersion;
    FCategories.Assign((Source as TvContact).Categories);
    FGeoLocation.Assign((Source as TvContact).GeoLocation);
    FNote.Assign((Source as TvContact).Note);
    FUpdated := (Source as TvContact).Updated;
    FSource := (Source as TvContact).Source;
    FPhoto.Assign((Source as TvContact).Photo);
    FPhotoURL := (Source as TvContact).PhotoURL;
    FPhotoEncoding := (Source as TvContact).PhotoEncoding;
    FPhotoType := (Source as TvContact).PhotoType;
    FSortString := (Source as TvContact).SortString;
    FTimeZone := (Source as TvContact).TimeZone;
    FProdID := (Source as TvContact).ProdID;
  end;
end;

const
  MinGraphicSize = 44;

function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    {$IFDEF DELPHIXE_LVL}
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
    {$ENDIF}
  else
    {$IFDEF DELPHIXE_LVL}
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else
    {$ENDIF}
    if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    {$IFDEF DELPHIXE_LVL}
    {$IFDEF DELPHIXE4_LVL}
    else if AnsiStrings.StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
      GraphicClass := TGIFImage
    {$ENDIF}
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;

function FindGraphicClass(Stream: TStream; out GraphicClass: TGraphicClass)
  : boolean; overload;
var
  Buffer: PByte;
  CurPos: Int64;
  BytesRead: Integer;
begin
  if Stream is TCustomMemoryStream then
  begin
    Buffer := TCustomMemoryStream(Stream).Memory;
    CurPos := Stream.Position;
    inc(Buffer, CurPos);
    Result := FindGraphicClass(Buffer^, Stream.Size - CurPos, GraphicClass);
    Exit;
  end;
  GetMem(Buffer, MinGraphicSize);
  try
    BytesRead := Stream.Read(Buffer^, MinGraphicSize);
    Stream.Seek(-BytesRead, soCurrent);
    Result := FindGraphicClass(Buffer^, BytesRead, GraphicClass);
  finally
    FreeMem(Buffer);
  end;
end;

procedure LoadPictureStream(Stream: TMemoryStream; Dest: TPicture);
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
begin

  if Stream.Size = 0 then
  begin
    Dest.Assign(nil);
    Exit;
  end;
  if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
    Exit; // raise EInvalidGraphic.Create('Invalid image');
  Graphic := GraphicClass.Create;
  try
    Stream.Position := 0;
    Graphic.LoadFromStream(Stream);
    Dest.Assign(Graphic);
  finally
    Graphic.Free;
  end;
end;

constructor TvContact.Create(Collection: TCollection);
begin
  inherited;
  FEmails := TvEmailCollection.Create(Self);
  FPhoneNumbers := TvPhoneCollection.Create(Self);
  FAddresses := TvAddressCollection.Create(Self);
  FCategories := TStringList.Create;
  FGeoLocation := TvGeoLocation.Create;
  FNote := TStringList.Create;
  FPhoto := TPicture.Create;
  FPhotoEncoding := feInternalBase64;
  FPhotoType := itJPEG;
  FvCardVersion := vv30;
  FSocialProfile := TvSocialProfile.Create(Self);
end;

procedure TvContact.DecodePhoto;
var
  ms: TMemoryStream;
  SourceString: Ansistring;
begin
  if PhotoStr <> '' then
  begin
    ms := TMemoryStream.Create;
    ms.Position := 0;
    SourceString := Decode64(AnsiString(PhotoStr));

    try
      ms.WriteBuffer(Pointer(SourceString)^, Length(SourceString));
      ms.Position := 0;
      SourceString := Decode64(AnsiString(PhotoStr));
      SetLength(SourceString, ms.Size);
      LoadPictureStream(ms, Photo);
      // ms.SaveToFile(FileName);
    finally
      ms.Free;
    end;
  end
  else
    Photo.Graphic := nil;
end;

destructor TvContact.Destroy;
begin
  FPhoto.Free;
  FEmails.Free;
  FPhoneNumbers.Free;
  FAddresses.Free;
  FvCardVersion := vv30;
  FCategories.Free;
  FGeoLocation.Free;
  FNote.Free;
  FSocialProfile.Free;
  inherited;
end;

procedure TvContact.EncodePhoto;
var
  LBuffer: TBytes;
  LFileStream: TMemoryStream;
  i: Integer;
  {$IFDEF DELPHI_UNICODE}
  LOffset: Integer;
  LEncoding, DestEncoding: TEncoding;
  {$ENDIF}
  strBuffer: string;
begin
  {$IFDEF DELPHI_UNICODE}
  LEncoding := nil;
  DestEncoding := TEncoding.Default;
  {$ENDIF}

  if Assigned(Photo.Graphic) and not(Photo.Graphic.Empty) then
  begin
    LFileStream := TMemoryStream.Create;
    try
      Photo.Graphic.SaveToStream(LFileStream);
      LFileStream.Position := 0;
      PhotoStr := '';
      SetLength(LBuffer, LFileStream.Size);
      LFileStream.ReadBuffer(Pointer(LBuffer)^, Length(LBuffer));
      {$IFDEF DELPHI_UNICODE}
      LOffset := TEncoding.GetBufferEncoding(LBuffer, LEncoding);
      LBuffer := LEncoding.Convert(LEncoding, DestEncoding, LBuffer, LOffset,
        Length(LBuffer) - LOffset);
      {$ENDIF}
      for i := 0 to Length(LBuffer) - 1 do
        strBuffer := strBuffer + chr(LBuffer[i]);

      PhotoStr := string(Encode64(string(strBuffer)));
     finally
       LFileStream.Free;
    end;
  end;
end;

procedure TvContact.SetAddresses(const Value: TvAddressCollection);
begin
  FAddresses.Assign(Value);
end;

procedure TvContact.SetBirthDay(const Value: TDateTime);
begin
  FBirthDay := Value;
end;

procedure TvContact.SetCategories(const Value: TStringList);
begin
  FCategories.Assign(Value);
end;

procedure TvContact.SetCompany(const Value: string);
begin
  FCompany := Value;
end;

procedure TvContact.SetEmails(const Value: TvEmailCollection);
begin
  FEmails.Assign(Value);
end;

procedure TvContact.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TvContact.SetFProfession(const Value: string);
begin
  FProfession := Value;
end;

procedure TvContact.SetFullName(const Value: string);
begin
  FFullName := Value;
end;

procedure TvContact.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TvContact.SetJobTitle(const Value: string);
begin
  FJobTitle := Value;
end;

procedure TvContact.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TvContact.SetMiddleName(const Value: string);
begin
  FMiddleName := Value;
end;

procedure TvContact.SetNamePrefix(const Value: string);
begin
  FNamePrefix := Value;
end;

procedure TvContact.SetNameSuffix(const Value: string);
begin
  FNameSuffix := Value;
end;

procedure TvContact.SetNickName(const Value: string);
begin
  FNickName := Value;
end;

procedure TvContact.SetNote(const Value: TStringList);
begin
  FNote.Assign(Value);
end;

procedure TvContact.SetPhotoURL(const Value: string);
begin
  FPhotoURL := Value;
end;

procedure TvContact.SetPhoneNumbers(const Value: TvPhoneCollection);
begin
  FPhoneNumbers := Value;
end;

procedure TvContact.SetPhoto(const Value: TPicture);
begin
  FPhoto.Assign(Value);
  if Assigned(Value) then
  begin
    {$IFDEF DELPHI_UNICODE}
    if FPhoto.Graphic is TJPEGImage then
      FPhotoType := itJPEG;
    {$ENDIF}
    {$IFDEF DELPHIXE_LVL}
    if FPhoto.Graphic is TGIFImage then
      FPhotoType := itGIF;
    if FPhoto.Graphic is TPNGImage then
      FPhotoType := itPNG;
    {$ENDIF}
  end;
end;

procedure TvContact.SetPhotoEncoding(const Value: TvFileEncoding);
begin
  FPhotoEncoding := Value;
end;

procedure TvContact.SetPhotoType(const Value: TvImageType);
begin
  FPhotoType := Value;
end;

procedure TvContact.SetSocialProfile(const Value: TvSocialProfile);
begin
  FSocialProfile.Assign(Value);
end;

procedure TvContact.SetSortString(const Value: string);
begin
  FSortString := Value;
end;

procedure TvContact.SetSource(const Value: string);
begin
  FSource := Value;
end;

procedure TvContact.SetTimeZone(const Value: string);
begin
  FTimeZone := Value;
end;

procedure TvContact.SetUpdated(const Value: TDateTime);
begin
  FUpdated := Value;
end;

procedure TvContact.SetvCardVersion(const Value: TvCardVersion);
begin
  FvCardVersion := Value;
end;

procedure TvContact.SetWebsiteURL(const Value: string);
begin
  FWebsiteURL := Value;
end;

{ TvContactsCollection }

function TvContactsCollection.Add: TvContact;
begin
  Result := TvContact(inherited Add);
end;

constructor TvContactsCollection.Create(AOwner: TvCard);
begin
  inherited Create(TvContact);
  FOwner := AOwner;
end;

function TvContactsCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TvContactsCollection.GetvContact(Index: Integer): TvContact;
begin
  Result := TvContact(inherited Items[Index]);
end;

function TvContactsCollection.Insert(Index: Integer): TvContact;
begin
  Result := TvContact(inherited Insert(Index));
end;

procedure TvContactsCollection.SetvContact(Index: Integer;
  const Value: TvContact);
begin
  inherited Items[Index] := Value;
end;

{ TvEmail }

procedure TvEmail.Assign(Source: TPersistent);
begin
  if (Source is TvEmail) then
  begin
    FEmailAddress := (Source as TvEmail).EmailAddress;
    FEmailType := (Source as TvEmail).EmailType;
    FPreferred := (Source as TvEmail).Preferred;
  end;
end;

constructor TvEmail.Create(Collection: TCollection);
begin
  inherited;

end;

destructor TvEmail.Destroy;
begin

  inherited;
end;

procedure TvEmail.SetEmailAddress(const Value: string);
begin
  FEmailAddress := Value;
end;

procedure TvEmail.SetEmailType(const Value: TvFieldType);
begin
  FEmailType := Value;
end;

procedure TvEmail.SetPreferred(const Value: boolean);
begin
  FPreferred := Value;
end;

{ TvEmailCollection }

function TvEmailCollection.Add: TvEmail;
begin
  Result := TvEmail(inherited Add);
end;

function TvEmailCollection.ToXml: String;
var
  doc: TXmlDocument;
  e0, e1, e2: IXmlNode;
  i: Integer;
begin
  doc := TXmlDocument.Create(TvContactsCollection(FOwner.Collection).FOwner);
  try
    doc.Active := true;
    e0 := doc.AddChild('webdavcollection');
    e1 := e0.AddChild('emails');
    for i := 0 to Count - 1 do
    begin
      e2 := e1.AddChild('email');
      e2.AddChild('emailaddress').Text := Self[i].EmailAddress;
      e2.AddChild('emailtype').Text :=
        FieldTypeToStr(Self[i].EmailType, vvNone);
      e2.AddChild('preferred').Text := BoolToStr(Self[i].FPreferred);
    end;
  finally
    Result := doc.XML.Text;
    doc.Free;
  end;
end;

procedure TvEmailCollection.LoadFromXml(XmlData: String);
var
  doc: TXmlDocument;
  i: Integer;
  K: Integer;
  C: Integer;
  item: TvEmail;
begin
  Clear;
  if XmlData <> '' then
  begin
    doc := TXmlDocument.Create(TvContactsCollection(FOwner.Collection).FOwner);
    try
      doc.LoadFromXml(XmlData);
      doc.Active := true;
      for i := 0 to doc.DocumentElement.ChildNodes.Count - 1 do
      begin
        if doc.DocumentElement.ChildNodes[i].LocalName = 'emails' then
        begin
          for K := 0 to doc.DocumentElement.ChildNodes[i]
            .ChildNodes.Count - 1 do
          begin
            if doc.DocumentElement.ChildNodes[i].ChildNodes[K].LocalName = 'email'
            then
            begin
              item := Add;
              for C := 0 to doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .ChildNodes.Count - 1 do
              begin
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'emailaddress' then
                  item.EmailAddress := doc.DocumentElement.ChildNodes[i]
                    .ChildNodes[K].ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'emailtype' then
                  item.EmailType :=
                    StrToFieldType(doc.DocumentElement.ChildNodes[i].ChildNodes
                    [K].ChildNodes[C].Text);
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'preferred' then
                  item.Preferred :=
                    StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text);
              end;
            end;
          end;
        end;
      end;
    finally
      doc.Free;
    end;
  end;
end;

constructor TvEmailCollection.Create(AOwner: TvContact);
begin
  inherited Create(TvEmail);
  FOwner := AOwner;
end;

function TvEmailCollection.GetEmail(Index: Integer): TvEmail;
begin
  Result := TvEmail(inherited Items[Index]);
end;

function TvEmailCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TvEmailCollection.Insert(Index: Integer): TvEmail;
begin
  Result := TvEmail(inherited Insert(Index));
end;

procedure TvEmailCollection.SetEmail(Index: Integer; const Value: TvEmail);
begin
  inherited Items[Index] := Value;
end;

{ TvPhone }

procedure TvPhone.Assign(Source: TPersistent);
begin
  if (Source is TvPhone) then
  begin
    FPhoneNumber := (Source as TvPhone).PhoneNumber;
    FPhoneType := (Source as TvPhone).PhoneType;
    FPreferred := (Source as TvPhone).Preferred;
    FFieldType := (Source as TvPhone).FieldType;
  end;
end;

{ TvPhoneCollection }

function TvPhoneCollection.Add: TvPhone;
begin
  Result := TvPhone(inherited Add);
end;

function TvPhoneCollection.ToXml: String;
var
  doc: TXmlDocument;
  e0, e1, e2: IXmlNode;
  i: Integer;
begin
  doc := TXmlDocument.Create(TvContactsCollection(FOwner.Collection).FOwner);
  try
    doc.Active := true;
    e0 := doc.AddChild('webdavcollection');
    e1 := e0.AddChild('phones');
    for i := 0 to Count - 1 do
    begin
      e2 := e1.AddChild('phone');
      e2.AddChild('phonenumber').Text := Self[i].FPhoneNumber;
      e2.AddChild('preferred').Text := BoolToStr(Self[i].FPreferred);
      e2.AddChild('fieldtype').Text :=
        FieldTypeToStr(Self[i].FFieldType, vvNone);
      e2.AddChild('IsText').Text := BoolToStr(ptText in Self[i].PhoneType);
      e2.AddChild('isvoice').Text := BoolToStr(ptVoice in Self[i].PhoneType);
      e2.AddChild('isfax').Text := BoolToStr(ptFax in Self[i].PhoneType);
      e2.AddChild('iscell').Text := BoolToStr(ptCell in Self[i].PhoneType);
      e2.AddChild('isvideo').Text := BoolToStr(ptVideo in Self[i].PhoneType);
      e2.AddChild('ispager').Text := BoolToStr(ptPager in Self[i].PhoneType);
      e2.AddChild('istextphone').Text :=
        BoolToStr(ptTextPhone in Self[i].PhoneType);
    end;
  finally
    Result := doc.XML.Text;
    doc.Free;
  end;
end;

procedure TvPhoneCollection.LoadFromXml(XmlData: String);
var
  doc: TXmlDocument;
  i: Integer;
  K: Integer;
  C: Integer;
  item: TvPhone;
begin
  Clear;
  if XmlData <> '' then
  begin
    doc := TXmlDocument.Create(TvContactsCollection(FOwner.Collection).FOwner);
    try
      doc.LoadFromXml(XmlData);
      doc.Active := true;
      for i := 0 to doc.DocumentElement.ChildNodes.Count - 1 do
      begin
        if doc.DocumentElement.ChildNodes[i].LocalName = 'phones' then
        begin
          for K := 0 to doc.DocumentElement.ChildNodes[i]
            .ChildNodes.Count - 1 do
          begin
            if doc.DocumentElement.ChildNodes[i].ChildNodes[K].LocalName = 'phone'
            then
            begin
              item := Add;
              item.FPhoneType := [];
              for C := 0 to doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .ChildNodes.Count - 1 do
              begin
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'phonenumber' then
                  item.PhoneNumber := doc.DocumentElement.ChildNodes[i]
                    .ChildNodes[K].ChildNodes[C].Text;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'istext') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptText];
                end;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'isvoice') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptVoice];
                end;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'isfax') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptFax];
                end;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'iscell') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptCell];
                end;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'isvideo') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptVideo];
                end;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'ispager') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptPager];
                end;
                if (doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes
                  [C].LocalName = 'istextphone') and
                  StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                  .ChildNodes[C].Text) then
                begin
                  item.FPhoneType := item.FPhoneType + [ptTextPhone];
                end;

                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'preferred' then
                  item.Preferred :=
                    StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text);
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'fieldtype' then
                  item.FFieldType :=
                    StrToFieldType(doc.DocumentElement.ChildNodes[i].ChildNodes
                    [K].ChildNodes[C].Text);
              end;
            end;
          end;
        end;
      end;
    finally
      doc.Free;
    end;
  end;
end;

constructor TvPhoneCollection.Create(AOwner: TvContact);
begin
  inherited Create(TvPhone);
  FOwner := AOwner;
end;

function TvPhoneCollection.GetItem(Index: Integer): TvPhone;
begin
  Result := TvPhone(inherited Items[Index]);
end;

function TvPhoneCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TvPhoneCollection.Insert(Index: Integer): TvPhone;
begin
  Result := TvPhone(inherited Insert(Index));
end;

procedure TvPhoneCollection.SetItem(Index: Integer; const Value: TvPhone);
begin
  inherited Items[Index] := Value;
end;

{ TvAddress }

procedure TvAddress.Assign(Source: TPersistent);
begin
  if (Source is TvAddress) then
  begin
    FPOBox := (Source as TvAddress).POBox;
    FNumber := (Source as TvAddress).Number;
    FStreet := (Source as TvAddress).Street;
    FCity := (Source as TvAddress).City;
    FRegion := (Source as TvAddress).Region;
    FPostCode := (Source as TvAddress).PostCode;
    FCountry := (Source as TvAddress).Country;
    FAddressType := (Source as TvAddress).AddressType;
    FMailingLabel.Assign((Source as TvAddress).MailingLabel);
    FPreferred := (Source as TvAddress).Preferred;
  end;
end;

constructor TvAddress.Create(Collection: TCollection);
begin
  inherited;
  FMailingLabel := TStringList.Create;
end;

destructor TvAddress.Destroy;
begin
  FMailingLabel.Free;
  inherited;
end;

procedure TvAddress.SetMailingLabel(const Value: TStringList);
begin
  FMailingLabel.Assign(Value);
end;

{ TvAddressCollection }

function TvAddressCollection.Add: TvAddress;
begin
  Result := TvAddress(inherited Add);
end;

function TvAddressCollection.ToXml: String;
var
  doc: TXmlDocument;
  e0, e1, e2: IXmlNode;
  i: Integer;
begin
  doc := TXmlDocument.Create(TvContactsCollection(FOwner.Collection).FOwner);
  try
    doc.Active := true;
    e0 := doc.AddChild('webdavcollection');
    e1 := e0.AddChild('addresses');
    for i := 0 to Count - 1 do
    begin
      e2 := e1.AddChild('address');
      e2.AddChild('street').Text := Self[i].FStreet;
      e2.AddChild('country').Text := Self[i].FCountry;
      e2.AddChild('number').Text := Self[i].FNumber;
      e2.AddChild('postcode').Text := Self[i].FPostCode;
      e2.AddChild('city').Text := Self[i].FCity;
      e2.AddChild('region').Text := Self[i].FRegion;
      e2.AddChild('addresstype').Text :=
        FieldTypeToStr(Self[i].FAddressType, vvNone);
      e2.AddChild('mailinglabel').Text := Self[i].FMailingLabel.Text;
      e2.AddChild('preferred').Text := BoolToStr(Self[i].FPreferred);
    end;
  finally
    Result := doc.XML.Text;
    doc.Free;
  end;
end;

procedure TvAddressCollection.LoadFromXml(XmlData: String);
var
  doc: TXmlDocument;
  i: Integer;
  K: Integer;
  C: Integer;
  item: TvAddress;
begin
  Clear;
  if XmlData <> '' then
  begin
    doc := TXmlDocument.Create(TvContactsCollection(FOwner.Collection).FOwner);
    try
      doc.LoadFromXml(XmlData);
      doc.Active := true;
      for i := 0 to doc.DocumentElement.ChildNodes.Count - 1 do
      begin
        if doc.DocumentElement.ChildNodes[i].LocalName = 'addresses' then
        begin
          for K := 0 to doc.DocumentElement.ChildNodes[i]
            .ChildNodes.Count - 1 do
          begin
            if doc.DocumentElement.ChildNodes[i].ChildNodes[K].LocalName = 'address'
            then
            begin
              item := Add;
              for C := 0 to doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .ChildNodes.Count - 1 do
              begin
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'number' then
                  item.Number := doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'street' then
                  item.Street := doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'country' then
                  item.Country := doc.DocumentElement.ChildNodes[i].ChildNodes
                    [K].ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'number' then
                  item.Number := doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'postcode' then
                  item.FPostCode := doc.DocumentElement.ChildNodes[i].ChildNodes
                    [K].ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'city' then
                  item.FCity := doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'pobox' then
                  item.FPOBox := doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'region' then
                  item.Region := doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'addresstype' then
                  item.AddressType :=
                    StrToFieldType(doc.DocumentElement.ChildNodes[i].ChildNodes
                    [K].ChildNodes[C].Text);
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'mailinglabel' then
                  item.MailingLabel.Text := doc.DocumentElement.ChildNodes[i]
                    .ChildNodes[K].ChildNodes[C].Text;
                if doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C]
                  .LocalName = 'preferred' then
                  item.FPreferred :=
                    StrToBool(doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                    .ChildNodes[C].Text);
              end;
            end;
          end;
        end;
      end;
    finally
      doc.Free;
    end;
  end;
end;

constructor TvAddressCollection.Create(AOwner: TvContact);
begin
  inherited Create(TvAddress);
  FOwner := AOwner;
end;

function TvAddressCollection.GetItem(Index: Integer): TvAddress;
begin
  Result := TvAddress(inherited Items[Index]);
end;

function TvAddressCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TvAddressCollection.Insert(Index: Integer): TvAddress;
begin
  Result := TvAddress(inherited Insert(Index));
end;

procedure TvAddressCollection.SetItem(Index: Integer; const Value: TvAddress);
begin
  inherited Items[Index] := Value;
end;

{ TvGeoLocation }

procedure TvGeoLocation.Assign(Source: TPersistent);
begin
  if (Source is TvGeoLocation) then
  begin
    FLatitude := (Source as TvGeoLocation).Latitude;
    FLongitude := (Source as TvGeoLocation).Longitude;
  end;
end;

{ TvSocialItem }

procedure TvSocialItem.Assign(Source: TPersistent);
begin
  if (Source is TvSocialItem) then
  begin
    FURI := (Source as TvSocialItem).URI;
    FUser := (Source as TvSocialItem).User;
    FService := (Source as TvSocialItem).Service;
  end;
end;

{ TvSocialProfile }

function TvSocialProfile.Add: TvSocialItem;
begin
  Result := TvSocialItem(inherited Add);
end;

constructor TvSocialProfile.Create(AOwner: TvContact);
begin
  inherited Create(TvSocialItem);
  FOwner := AOwner;
end;

function TvSocialProfile.GetItem(Index: Integer): TvSocialItem;
begin
  Result := TvSocialItem(inherited Items[Index]);
end;

function TvSocialProfile.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TvSocialProfile.Insert(Index: Integer): TvSocialItem;
begin
  Result := TvSocialItem(inherited Insert(Index));
end;

procedure TvSocialProfile.SetItem(Index: Integer; const Value: TvSocialItem);
begin
  inherited Items[Index] := Value;
end;

end.
