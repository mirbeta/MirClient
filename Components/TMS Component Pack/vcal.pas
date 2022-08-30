{***************************************************************************}
{ TvCalendar component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001-2013                                          }
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

unit vCal;

interface

{$I TMSDEFS.INC}

uses
  Classes, SysUtils, Windows;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : Property vCalendarVersion property added
  // v1.0.1.1 : Fixed  Issue with attributes in value specifiers
  // v1.1.0.0 : New : Support for parsing timezone information
  // v1.2.0.0 : New : SaveToStream, LoadFromStream, InsertFromStream added

type
  TvCalendar = class;

  TvEventStatus = (esAccepted,esNeedsAction,esSent,esTentative,
                   esConfirmed,esDeclined,esCompleted,esDelegated);

  TvCalenderVersion = (vv1, vv2);

  TvEventTransp = (etFree,etNotFree,etOther);

  TvEventClass = (ecPublic, ecPrivate, ecConfidential);

  TvEventCategory = (caAppointment,caBusiness,caEducation,caHoliday,
                     caMeeting,caMiscellaneous,caPersonal,caPhonecall,
                     caSickDay,caSpecialOccasion,caTravel,caVacation);

  TEventProperty = (epDTStart,epDTEnd,epStatus,epPriority,epSummary,epTransp,epDescription,
                    epURL,epUID,epLocation,epClass,epCategories,epResources, epRecurrency);

  TvEventCategories = set of TvEventCategory;

  TvTimeZoneOffset = class(TPersistent)
  private
    FOffsetFrom: integer;
    FDTStart: TDateTime;
    FOffsetTo: integer;
    FName: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OffsetFrom: integer read FOffsetFrom write FOffsetFrom;
    property OffsetTo: integer read FOffsetTo write FOffsetTo;
    property DTStart: TDateTime read FDTStart write FDTStart;
    property Name: string read FName write FName;
  end;

  TvTimeZone = class(TPersistent)
  private
    FOwner: TvCalendar;
    FID: string;
    FDayLight: TvTimeZoneOffset;
    FURL: string;
    FStandard: TvTimeZoneOffset;
    procedure SetDaylight(const Value: TvTimeZoneOffset);
    procedure SetStandard(const Value: TvTimeZoneOffset);
  public
    constructor Create(Owner: TvCalendar);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ToXml: String;
    procedure LoadFromXml(XMLData: String);
  published
    property ID: string read FID write FID;
    property URL: string read FURL write FURL;
    property Standard: TvTimeZoneOffset read FStandard write SetStandard;
    property Daylight: TvTimeZoneOffset read FDayLight write SetDaylight;
  end;

  TvEvent = class(TCollectionItem)
  private
    FDTEnd: TDateTime;
    FDTStart: TDateTime;
    FPriority: Integer;
    FStatus: TvEventStatus;
    FSummary: string;
    FTrans: TvEventTransp;
    FDescription: TStrings;
    FURL: string;
    FUID: string;
    FLocation: string;
    FClass: TvEventClass;
    FCategories: TvEventCategories;
    FResources: TStrings;
    FAlarmMessage: string;
    FAlarmTime: TDateTime;
    FRecurrency: string;
    FvTimeZone: TvTimeZone;
    procedure SetDTEnd(const Value: TDateTime);
    procedure SetDTStart(const Value: TDateTime);
    procedure SetPriority(const Value: Integer);
    procedure SetStatus(const Value: TvEventStatus);
    procedure SetSummary(const Value: string);
    procedure SetTransp(const Value: TvEventTransp);
    procedure SetDescription(const Value: TStrings);
    procedure SetUID(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetLocation(const Value: string);
    procedure SetClass(const Value: TvEventClass);
    procedure SetCategories(const Value: TvEventCategories);
    procedure SetResources(const Value: TStrings);
    procedure SetRecurrency(const Value: string);
    procedure SetvTimeZone(const Value: TvTimeZone);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AlarmMessage: string read FAlarmMessage write FAlarmMessage;
    property AlarmTime: TDateTime read FAlarmTime write FAlarmTime;
    property Categories: TvEventCategories read FCategories write SetCategories;
    property Classification: TvEventClass read FClass write SetClass;
    property Description: TStrings read FDescription write SetDescription;
    property DTEnd: TDateTime read FDTEnd write SetDTEnd;
    property DTStart: TDateTime read FDTStart write SetDTStart;
    property Location: string read FLocation write SetLocation;
    property Priority: Integer read FPriority write SetPriority;
    property Recurrency: string read FRecurrency write SetRecurrency;
    property Resources: TStrings read FResources write SetResources;
    property Status: TvEventStatus read FStatus write SetStatus;
    property Summary: string read FSummary write SetSummary;
    property Transp: TvEventTransp read FTrans write SetTransp;
    property URL: string read FURL write SetURL;
    property UID: string read FUID write SetUID;
    property vTimeZone: TvTimeZone read FvTimeZone write SetvTimeZone;
  end;


  TvEventCollection = class(TCollection)
  private
    FOwner: TvCalendar;
    function GetvEvent(Index: Integer): TvEvent;
    procedure SetvEvent(Index: Integer; const Value: TvEvent);
  public
    constructor Create(AOwner: TvCalendar);
    function Add:TvEvent;
    function Insert(Index: Integer): TvEvent;
    property Items[Index: Integer]: TvEvent read GetvEvent write SetvEvent; default;
    function GetOwner: TPersistent; override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TvCalendar = class(TComponent)
  private
    FvEvents: TvEventCollection;
    FProdID: string;
    FTimeZone: string;
    FvTimeZone: TvTimeZone;
    FvCalendarVersion: TvCalenderVersion;
    function GetvEvents: TvEventCollection;
    procedure SetvEvents(const Value: TvEventCollection);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetTimeZone(const Value: TvTimeZone);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure InsertFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure InsertFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property vTimeZone: TvTimeZone read FvTimeZone write SetTimeZone;

  published
    property vEvents: TvEventCollection read GetvEvents write SetvEvents;
    property ProdID: string read FProdID write FProdID;
    property TimeZone: string read FTimeZone write FTimeZone;
    property vCalendarVersion: TvCalenderVersion read FvCalendarVersion write FvCalendarVersion default vv1;
    property Version: string read GetVersion write SetVersion;
  end;

function StatusToStr(AStatus: TvEventStatus): string;
function StrToEventCategory(Value: String): TvEventCategory;
function EventCategoryToStr(Category: TvEventCategory): string;
function EventPropertyToString(Value: TEventProperty): String;
function StringToEventProperty(Value: String): TEventProperty;
function EventStatusToString(Value: TvEventStatus): String;
function StringToEventStatus(Value: String): TvEventStatus;
function EventClassToStr(Value: TvEventClass): String;
function StrToEventClass(Value: String): TvEventClass;

implementation

uses
  XMLDoc, XmlIntf;

function EventClassToStr(Value: TvEventClass): String;
begin
  case Value of
    ecPublic:
      Result := 'public';
    ecPrivate:
      Result := 'private';
    ecConfidential:
      Result := 'confidential';
  end;
end;

function StrToEventClass(Value: String): TvEventClass;
begin
  Result := ecPublic;
  if Value = 'private' then
    Result := ecPrivate;
  if Value = 'confidential' then
    Result := ecConfidential;
end;

function EventPropertyToString(Value: TEventProperty): String;
begin
  case Value of
    epDTStart:
      Result := 'start';
    epDTEnd:
      Result := 'end';
    epStatus:
      Result := 'status';
    epPriority:
      Result := 'priority';
    epSummary:
      Result := 'summary';
    epTransp:
      Result := 'transp';
    epDescription:
      Result := 'description';
    epURL:
      Result := 'url';
    epUID:
      Result := 'uid';
    epLocation:
      Result := 'location';
    epClass:
      Result := 'class';
    epCategories:
      Result := 'categories';
    epResources:
      Result := 'resources';
    epRecurrency:
      Result := 'recurrency';
  end;
end;

function StringToEventProperty(Value: String): TEventProperty;
begin
  Result := epDTStart;

  if Value = 'end' then
    Result := epDTEnd;
  if Value = 'status' then
    Result := epStatus;
  if Value = 'prioerity' then
    Result := epPriority;
  if Value = 'summary' then
    Result := epSummary;
  if Value = 'transp' then
    Result := epTransp;
  if Value = 'description' then
    Result := epDescription;
  if Value = 'url' then
    Result := epURL;
  if Value = 'Result' then
    Result := epUID;
  if Value = 'Location' then
    Result := epLocation;
  if Value = 'class' then
    Result := epClass;
  if Value = 'categories' then
    Result := epClass;
  if Value = 'resources' then
    Result := epResources;
  if Value = 'recurrency' then
    Result := epRecurrency;
end;

function EventStatusToString(Value: TvEventStatus): String;
begin
  case Value of
    esAccepted:
      Result := 'accepted';
    esNeedsAction:
      Result := 'needsaction';
    esSent:
      Result := 'sent';
    esTentative:
      Result := 'tentative';
    esConfirmed:
      Result := 'confirmed';
    esDeclined:
      Result := 'declined';
    esCompleted:
      Result := 'completed';
    esDelegated:
      Result := 'delegated';
  end;
end;

function StringToEventStatus(Value: String): TvEventStatus;
begin
  Result := esAccepted;
  if Value = 'needsaction' then
    Result := esNeedsAction;
  if Value = 'sent' then
    Result := esSent;
  if Value = 'tentative' then
    Result := esTentative;
  if Value = 'confirmed' then
    Result := esConfirmed;
  if Value = 'declined' then
    Result := esDeclined;
  if Value = 'completed' then
    Result := esCompleted;
  if Value = 'delegated' then
    Result := esDelegated;
end;

function EventTransPToString(Value: TvEventTransp): String;
begin
  case Value of
    etFree:
      Result := 'free';
    etNotFree:
      Result := 'notfree';
    etOther:
      Result := 'other';
  end;
end;

function StringToEventTrans(Value: String): TvEventTransp;
begin
  Result := etFree;
  if Value = 'notfree' then
    Result := etNotFree;
  if Value = 'other' then
    Result := etOther;
end;

function StrToEventCategory(Value: String): TvEventCategory;
begin
  Result := caAppointment;
  if Value = 'business' then
    Result := caBusiness;
  if Value = 'education' then
    Result := caEducation;
  if Value = 'holiday' then
    Result := caHoliday;
  if Value = 'meeting' then
    Result := caMeeting;
  if Value = 'miscellaneous' then
    Result := caMiscellaneous;
  if Value = 'personal' then
    Result := caPersonal;
  if Value = 'phonecall' then
    Result := caPhonecall;
  if Value = 'sickday' then
    Result := caSickDay;
  if Value = 'special occasion' then
    Result := caSpecialOccasion;
  if Value = 'travel' then
    Result := caTravel;
  if Value = 'vacation' then
    Result := caVacation;
end;

function EventCategoryToStr(Category: TvEventCategory): string;
begin
  case Category of
    caAppointment:
      Result := 'appointment';
    caBusiness:
      Result := 'business';
    caEducation:
      Result := 'education';
    caHoliday:
      Result := 'holiday';
    caMeeting:
      Result := 'meeting';
    caMiscellaneous:
      Result := 'miscellaneous';
    caPersonal:
      Result := 'personal';
    caPhonecall:
      Result := 'phonecall';
    caSickDay:
      Result := 'sickday';
    caSpecialOccasion:
      Result := 'specialoccasion';
    caTravel:
      Result := 'travel';
    caVacation:
      Result := 'vacation';
  end;
end;

type
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


{ Utility functions }

function IntToZStr(i,l: Integer):string;
var
  Res: string;
begin
  Res := IntToStr(i);
  while Length(Res)<l do
    Res := '0' + Res;

  Result := Res;
end;

function IsoToOffset(s: string): integer;
var
  fct: integer;
  ho,mi,err: integer;
begin
  fct := 1;

  if pos('-',s) = 1 then
  begin
    delete(s,1,1);
    fct := -1;
  end;

  if pos('+',s) = 1 then
  begin
    delete(s,1,1);
    fct := +1;
  end;

  Val(Copy(s,1,2),ho,err);
  Val(Copy(s,3,2),mi,err);

  Result := fct * (ho * 60) + mi;
end;

function IsoToDateTime(s: string):TDateTime;
var
  da,mo,ye,ho,mi,se: Word;
  err: Integer;
begin
  Val(Copy(s,1,4),ye,err);
  Val(Copy(s,5,2),mo,err);
  Val(Copy(s,7,2),da,err);
  Val(Copy(s,10,2),ho,err);
  Val(Copy(s,12,2),mi,err);
  Val(Copy(s,14,2),se,err);
  Result := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
end;

function DateTimeToIso(dt: TDateTime):string;
var
  da,mo,ye,ho,mi,se,ms:word;
begin
  DecodeDate(dt,ye,mo,da);
  DecodeTime(dt,ho,mi,se,ms);
  Result := IntToStr(ye) + IntToZStr(mo,2) + IntToZStr(da,2) + 'T' +
            IntToZStr(ho,2) + IntToZStr(mi,2) + IntToZStr(se,2);
end;

function StatusToStr(AStatus: TvEventStatus):string;
begin
  case AStatus of
  esAccepted: result := 'ACCEPTED';
  esNeedsAction: result := 'NEEDS ACTION';
  esSent: result := 'SENT';
  esTentative: result := 'TENTATIVE';
  esConfirmed: result := 'CONFIRMED';
  esDeclined: result := 'DECLINED';
  esCompleted: result := 'COMPLETED';
  esDelegated: result := 'DELEGATED';
  end;

end;

function StrToStatus(Value: string):TvEventStatus;
begin
  Result := esNeedsAction; //defined default value

  if UpperCase(Value) = 'ACCEPTED' then
    Result := esAccepted;
  if UpperCase(Value) = 'NEEDS ACTIONS' then
    Result := esNeedsAction;
  if UpperCase(Value) = 'SENT' then
    Result := esSent;
  if UpperCase(Value) = 'TENTATIVE' then
    Result := esTentative;
  if UpperCase(Value) = 'CONFIRMED' then
    Result := esConfirmed;
  if UpperCase(Value) = 'DECLINED' then
    Result := esDeclined;
  if UpperCase(Value) = 'COMPLETED' then
    Result := esCompleted;
  if UpperCase(Value) = 'DELEGATED' then
    Result := esDelegated;
end;

function StrToClass(Value: string):TvEventClass;
begin
  Result := ecPublic;
  if Uppercase(Value) = 'PUBLIC' then
    Result := ecPublic;
  if Uppercase(Value) = 'PRIVATE' then
    Result := ecPrivate;
  if Uppercase(Value) = 'CONFIDENTIAL' then
    Result := ecConfidential;
end;

function StrToCat(Value: string):TvEventCategories;
begin
  Result := [];
  Value := Uppercase(Value);

  if Pos('APPOINTMENT',Value) > 0 then
    Result := Result + [caAppointment];

  if Pos('BUSINESS',Value) > 0 then
    Result := Result + [caBusiness];

  if Pos('EDUCATION',Value) > 0 then
    Result := Result + [caEducation];

  if Pos('HOLIDAY',Value) > 0 then
    Result := Result + [caHoliday];

  if Pos('MEETING',Value) > 0 then
    Result := Result + [caMeeting];

  if Pos('MISCELLANEOUS',Value) > 0 then
    Result := Result + [caMiscellaneous];

  if Pos('PERSONAL',Value) > 0 then
    Result := Result + [caPersonal];

  if Pos('PHONE CALL',Value) > 0 then
    Result := Result + [caPhonecall];

  if Pos('SICK DAY',Value) > 0 then
    Result := Result + [caSickDay];

  if Pos('SPECIAL OCCASION',Value) > 0 then
    Result := Result + [caSpecialOccasion];

  if Pos('TRAVEL',Value) > 0 then
    Result := Result + [caTravel];

  if Pos('VACATION',Value) > 0 then
    Result := Result + [caVacation];

end;

function ClassToStr(Value: TvEventClass):string;
begin
  case Value of
  ecPublic: Result := 'PUBLIC';
  ecPrivate: Result := 'PRIVATE';
  ecConfidential: Result := 'CONFIDENTIAL';
  else
    Result := 'PUBLIC';
  end;
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function ConvertFromCRLF(s:string):string;
begin
  s := StringReplace(s,'=0D',#13,[rfReplaceAll]);
  s := StringReplace(s,'=0A',#10,[rfReplaceAll]);
  s := StringReplace(s,'=3D','=',[rfReplaceAll]);
  Result := s;
end;

function ConvertToCRLF(s:string):string;
begin
  s := StringReplace(s, '=','=3D',[rfReplaceAll]);
  s := StringReplace(s, #13,'=0D',[rfReplaceAll]);
  s := StringReplace(s, #10,'=0A',[rfReplaceAll]);
  Result := s;
end;

function DelSpaces(s:string):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] <> ' ' then Result := Result + s[i];
end;

function GetPropVal(s:string):string;
begin
  Result := '';

  if pos('"', s) > 0 then
  begin
    Delete(s,1,pos('"',s));
    if pos('"', s) > 0 then
      Delete(s,1,pos('"',s));
  end;

  if Pos(':',s) > 0 then
  begin
    Delete(s,1,pos(':',s));
    Result := Trim(s);
  end;
end;

function LastChar(s:string):char;
begin
  Result := #0;
  if Length(s) > 0 then
    Result := s[Length(s)];
end;

function TrimFolding(s:string):string;
begin
  if LastChar(s) = '=' then
    Delete(s,Length(s),1);
  Result := s;
end;


{ TvCalendar }

constructor TvCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FvEvents := TvEventCollection.Create(Self);
  FvTimeZone := TvTimeZone.Create(Self);
end;

destructor TvCalendar.Destroy;
begin
  FvEvents.Free;
  FvTimeZone.Free;
  inherited;
end;

function TvCalendar.GetvEvents: TvEventCollection;
begin
  Result := FvEvents;
end;

procedure TvCalendar.LoadFromFile(const FileName: string);
begin
  vEvents.Clear;
  InsertFromFile(FileName);
end;

procedure TvCalendar.LoadFromStream(Stream: TStream);
begin
  vEvents.Clear;
  InsertFromStream(Stream);
end;

procedure TvCalendar.InsertFromFile(const FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    fs.Position := 0;
    InsertFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TvCalendar.InsertFromStream(Stream: TStream);
var
  s: string;
  vEvent: TvEvent;
  Err, V, offi: Integer;
  LastProp: TEventProperty;
  Folded, FoldingChar: boolean;
  isTZ,isSTD,isDAY: boolean;
  dt: TDateTime;
  sl: TFileStringList;

begin
  sl := TFileStringList.Create;
  {$IFDEF DELPHI_UNICODE}
  sl.LoadFromStream(Stream,TEncoding.UTF8);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  sl.LoadFromStream(Stream);
  {$ENDIF}

  try
    vEvent := Nil;
    FoldingChar := False;
    LastProp := epUID;
    isTZ := false;
    isSTD := false;
    isDAY := false;

    while not sl.Eof do
    begin
      sl.ReadLn(s);

      Folded := (Pos(' ',s) = 1) or FoldingChar;

      FoldingChar := False;

      s := ConvertFromCRLF(s);

      if Pos('BEGIN:VTIMEZONE',DelSpaces(s)) = 1 then
      begin
        isTZ := true;
      end;

      if Pos('END:VTIMEZONE',DelSpaces(s)) = 1 then
      begin
        isTZ := false;
      end;

      if isTZ then
      begin
        if Pos('BEGIN:STANDARD',DelSpaces(s)) = 1 then
        begin
          isSTD := true;
        end;

        if Pos('END:STANDARD',DelSpaces(s)) = 1 then
        begin
          isSTD := false;
        end;

        if Pos('BEGIN:DAYLIGHT',DelSpaces(s)) = 1 then
        begin
          isDAY := true;
        end;

        if Pos('END:DAYLIGHT',DelSpaces(s)) = 1 then
        begin
          isDAY := false;
        end;

        if (Pos('TZID',s) = 1) then
        begin
          FvTimeZone.ID := GetPropVal(s);
        end;

        if (Pos('TZURL',s) = 1) then
        begin
          FvTimeZone.URL := GetPropVal(s);
        end;

        if (Pos('DTSTART',s) = 1) then
        begin
          dt := IsoToDateTime(GetPropVal(s));
           if isSTD then
          begin
            FvTimeZone.Standard.DTStart := dt;
          end;
          if isDAY then
          begin
            FvTimeZone.DayLight.DTStart := dt;
          end;
        end;

        if (Pos('TZOFFSETFROM',s) = 1) then
        begin
          offi := IsoToOffset(GetPropVal(s));
          if isSTD then
          begin
            FvTimeZone.Standard.OffsetFrom := offi;
          end;
          if isDAY then
          begin
            FvTimeZone.DayLight.OffsetFrom := offi;
          end;
        end;

        if (Pos('TZOFFSETTO',s) = 1) then
        begin
          offi := IsoToOffset(GetPropVal(s));
          if isSTD then
          begin
            FvTimeZone.Standard.OffsetTo := offi;
          end;
          if isDAY then
          begin
            FvTimeZone.DayLight.OffsetTo := offi;
          end;
        end;

        if (Pos('TZNAME',s) = 1) then
        begin
          if isSTD then
          begin
            FvTimeZone.Standard.Name := GetPropVal(s);
          end;
          if isDAY then
          begin
            FvTimeZone.DayLight.Name := GetPropVal(s);
          end;
        end;
      end;

      if not isTZ then
      begin

        if Pos('BEGIN:VEVENT',DelSpaces(s)) = 1 then
        begin
          vEvent := vEvents.Add;
          vEvent.vTimeZone.Assign(vTimeZone);
        end;

        if Pos('END:VEVENT',DelSpaces(s)) = 1 then
        begin
          vEvent := Nil;
        end;

        if (Pos('DTSTART',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.DTStart := IsoToDateTime(GetPropVal(s));
          LastProp := epDTStart;
        end;

        if (Pos('DTEND',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.DTEnd := IsoToDateTime(GetPropVal(s));
          LastProp := epDTEnd;
        end;

        if (Pos('STATUS',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.Status := StrToStatus(GetPropVal(s));
          LastProp := epStatus;
        end;

        if (Pos('PRIORITY',s) = 1) and Assigned(vEvent) then
        begin
          s := GetPropVal(s);
          val(s, V, Err);
          vEvent.Priority := V;
          LastProp := epPriority;
        end;

        if ((Pos('SUMMARY',s) = 1) or
           (Folded and (LastProp = epSummary))) and Assigned(vEvent) then
        begin
          FoldingChar := LastChar(s) = '=';
          s := TrimFolding(s);

          if Folded then
            vEvent.Summary := vEvent.Summary + ' '+Trim(s)
          else
            vEvent.Summary := GetPropVal(s);

          LastProp := epSummary;
        end;

        if (Pos('TRANSP',s) = 1) and Assigned(vEvent) then
        begin
          s := GetPropVal(s);
          val(s, V, Err);
          case V of
          0: vEvent.Transp := etFree;
          1: vEvent.Transp := etNotFree;
          else
            vEvent.Transp := etOther;
          end;
          LastProp := epTransp;
        end;

        if ((Pos('DESCRIPTION',s) = 1) or
           (Folded and (LastProp = epDescription))) and Assigned(vEvent) then
        begin
          FoldingChar := LastChar(s) = '=';
          s := TrimFolding(s);

          if Folded then
            vEvent.Description.Strings[0] := vEvent.Description.Strings[0] + Trim(s)
          else
            vEvent.Description.Add(GetPropVal(s));

          LastProp := epDescription;
        end;

        if (Pos('URL',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.URL := GetPropVal(s);
          LastProp := epURL;
        end;

        if (Pos('UID',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.UID := GetPropVal(s);
          LastProp := epUID;
        end;

        if (Pos('LOCATION',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.Location := GetPropVal(s);
          LastProp := epLocation;
        end;

        if (Pos('CLASS',s) = 1) and Assigned(vEvent) then
        begin
          s := GetPropVal(s);
          vEvent.Classification := StrToClass(s);
          LastProp := epClass;
        end;

        if (Pos('CATEGORIES',s) = 1) and Assigned(vEvent) then
        begin
          s := GetPropVal(s);
          vEvent.Categories := StrToCat(s);
          LastProp := epCategories;
        end;

        if (Pos('RESOURCES',s) = 1) and Assigned(vEvent) then
        begin
          s := GetPropVal(s);
          while Pos(';',s) > 0 do
          begin
            vEvent.Resources.Add(Copy(s,1,Pos(';',s)-1));
            Delete(s,1,Pos(';',s));
          end;
          if s <> '' then vEvent.Resources.Add(s);
          LastProp := epResources;
        end;

        if (Pos('RRULE',s) = 1) and Assigned(vEvent) then
        begin
          vEvent.Recurrency := GetPropVal(s);
          LastProp := epLocation;
        end;
      end;

    end;
  finally
    sl.Free;
  end;
end;

procedure TvCalendar.SaveToFile(const FileName: string);
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

procedure TvCalendar.SaveToStream(Stream: TStream);
var
  i,j: Integer;
  Cat,Res: string;
  sl: TFileStringList;
  {$IFDEF DELPHI_UNICODE}
  S:AnsiString;
  {$ENDIF}
begin
  sl := TFileStringList.Create;
  Stream.Size:=0;
  try
    sl.Writeln('BEGIN:VCALENDAR');
    if FProdID <> '' then
      sl.Writeln('PRODID:' + FProdID);
    if FTimeZone <> '' then
      sl.Writeln('TZ:' + FTimeZone);

    case FvCalendarVersion of
      vv1: sl.Writeln('VERSION:1.0');
      vv2: sl.Writeln('VERSION:2.0');
    end;

    for i := 1 to vEvents.Count do
      with vEvents.Items[i - 1] do
      begin
        sl.WriteLn('BEGIN:VEVENT');
        sl.WriteLn('DTSTART:' + DateTimeToIso(DTStart));
        sl.WriteLn('DTEND:' + DateTimeToIso(DTEnd));
        sl.WriteLn('PRIORITY:' + IntToStr(Priority));
        sl.WriteLn('STATUS:' + StatusToStr(Status));
        sl.WriteLn('CLASS:' + ClassToStr(Classification));
        sl.WriteLn('SUMMARY:'+ ConvertToCRLF(Summary));
        sl.WriteLn('TRANSP:'+IntToStr(Ord(Transp)));
        if UID <> '' then
          sl.WriteLn('UID:' + UID);
        if URL <> '' then
          sl.WriteLn('URL:' + URL);
        if Location <> '' then
          sl.WriteLn('LOCATION:' + Location);

        if Recurrency <> '' then
          sl.WriteLn('RRULE:' + Recurrency);

        if (AlarmMessage <> '') and (AlarmTime <> 0) then
        begin
          {
          sl.WriteLn('BEGIN:VALARM');
          sl.WriteLn('TRIGGER:-PT30M');
          sl.WriteLn('ACTION:DISPLAY');
          sl.WriteLn('DESCRIPTION:Reminder');
          sl.WriteLn('END:VALARM');
          }
          sl.WriteLn('DALARM:'+DateTimeToIso(AlarmTime)+';PT5M;2;'+ConvertToCRLF(AlarmMessage));
        end;

        Cat := '';
        if caAppointment in Categories then
          Cat := 'APPOINTMENT';
        if caBusiness in Categories then
          Cat := Cat + ';BUSINESS';
        if caEducation in Categories then
          Cat := Cat + ';EDUCATION';
        if caHoliday in Categories then
          Cat := Cat + ';HOLIDAY';
        if caMeeting in Categories then
          Cat := Cat + ';MEETING';
        if caMiscellaneous in Categories then
          Cat := Cat + ';MISCELLANEOUS';
        if caPersonal in Categories then
          Cat := Cat + ';PERSONAL';
        if caPhonecall in Categories then
          Cat := Cat + ';PHONE CALL';
        if caSickDay in Categories then
          Cat := Cat + ';SICK DAY';
        if caSpecialOccasion in Categories then
          Cat := Cat + ';SPECIAL OCCASION';
        if caTravel in Categories then
          Cat := Cat + ';TRAVEL';
        if caVacation in Categories then
          Cat := Cat + ';VACATION';

        if Pos(';',Cat) = 1 then Delete(Cat,1,1);
        if Cat <> '' then
          sl.WriteLn('CATEGORIES:' + Cat);

        if Resources.Count > 0 then
        begin
          Res := '';
          for j := 1 to Resources.Count do
            if Res = '' then
              Res := Resources.Strings[j - 1]
            else
              Res := Res + ';' + Resources.Strings[j - 1];
          sl.WriteLn('RESOURCES:'+Res);
        end;

        for j := 1 to Description.Count do
        begin
          Res := '';
          if j = 1 then
            Res := 'DESCRIPTION;ENCODING=QUOTED-PRINTABLE:'+ConvertToCRLF(Description.Strings[j - 1])
          else
            Res := ' '+ConvertToCRLF(Description.Strings[j - 1]);

          if j < Description.Count then
            Res := Res + '=0D=0A=';

          sl.WriteLn(Res);

        end;

        sl.WriteLn('END:VEVENT');
      end;

    sl.WriteLn('END:VCALENDAR');
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

procedure TvCalendar.SetvEvents(const Value: TvEventCollection);
begin
  FvEvents.Assign(Value);
end;

function TvCalendar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TvCalendar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TvCalendar.SetTimeZone(const Value: TvTimeZone);
begin
  FvTimeZone.Assign(Value);
end;

procedure TvCalendar.SetVersion(const Value: string);
begin

end;

{ TvEventCollection }

function TvEventCollection.Add: TvEvent;
begin
  Result := TvEvent(inherited Add);
end;

constructor TvEventCollection.Create(AOwner: TvCalendar);
begin
  inherited Create(TvEvent);
  FOwner := AOwner;
end;

function TvEventCollection.GetOwner: TPersistent;
begin
  Result := TPersistent(FOwner);
end;

function TvEventCollection.GetvEvent(Index: Integer): TvEvent;
begin
  Result := TvEvent(inherited Items[Index]);
end;

function TvEventCollection.Insert(Index: Integer): TvEvent;
begin
  Result := TvEvent(inherited Insert(Index));
end;

procedure TvEventCollection.SetvEvent(Index: Integer;
  const Value: TvEvent);
begin
  inherited SetItem(Index,Value);
end;

{ TvEvent }

procedure TvEvent.Assign(Source: TPersistent);
begin
  if (Source is TvEvent) then
  begin
    FAlarmMessage := (Source as TvEvent).AlarmMessage;
    FAlarmTime := (Source as TvEvent).AlarmTime;
    FCategories := (Source as TvEvent).Categories;
    FClass := (Source as TvEvent).Classification;
    FDescription.Assign((Source as TvEvent).Description);
    FDTEnd := (Source as TvEvent).DTEnd;
    FDTStart := (Source as TvEvent).DTStart;
    FLocation := (Source as TvEvent).Location;
    FPriority := (Source as TvEvent).Priority;
    FRecurrency := (Source as TvEvent).Recurrency;
    FResources.Assign((Source as TvEvent).Resources);
    FStatus := (Source as TvEvent).Status;
    FSummary := (Source as TvEvent).Summary;
    FTrans := (Source as TvEvent).Transp;
    FURL := (Source as TvEvent).URL;
    FUID := (Source as TvEvent).UID;
    FvTimeZone.Assign((Source as TvEvent).vTimeZone);
  end;
end;

constructor TvEvent.Create(Collection: TCollection);
begin
  inherited;
  FDescription := TStringList.Create;
  FResources := TStringList.Create;
  FStatus := esNeedsAction;
  FvTimeZone := TvTimeZone.Create(TvEventCollection(Collection).FOwner);
end;

destructor TvEvent.Destroy;
begin
  FResources.Free;
  FvTimeZone.Free;
  FDescription.Free;
  inherited;
end;

procedure TvEvent.SetCategories(const Value: TvEventCategories);
begin
  FCategories := Value;
end;

procedure TvEvent.SetClass(const Value: TvEventClass);
begin
  FClass := Value;
end;

procedure TvEvent.SetDescription(const Value: TStrings);
begin
  FDescription.Assign(Value);
end;

procedure TvEvent.SetDTEnd(const Value: TDateTime);
begin
  FDTEnd := Value;
end;

procedure TvEvent.SetDTStart(const Value: TDateTime);
begin
  FDTStart := Value;
end;

procedure TvEvent.SetLocation(const Value: string);
begin
  FLocation := Value;
end;

procedure TvEvent.SetPriority(const Value: Integer);
begin
  FPriority := Value;
end;

procedure TvEvent.SetRecurrency(const Value: string);
begin
  FRecurrency := Value;
end;

procedure TvEvent.SetResources(const Value: TStrings);
begin
  FResources.Assign(Value);
end;

procedure TvEvent.SetStatus(const Value: TvEventStatus);
begin
  FStatus := Value;
end;

procedure TvEvent.SetSummary(const Value: string);
begin
  FSummary := Value;
end;

procedure TvEvent.SetTransp(const Value: TvEventTransp);
begin
  FTrans := Value;
end;

procedure TvEvent.SetUID(const Value: string);
begin
  FUID := Value;
end;

procedure TvEvent.SetURL(const Value: string);
begin
  FURL := Value;
end;

procedure TvEvent.SetvTimeZone(const Value: TvTimeZone);
begin
  FvTimeZone.Assign(Value);
end;

{ TvTimeZoneOffset }

procedure TvTimeZoneOffset.Assign(Source: TPersistent);
begin
  if (Source is TvTimeZoneOffset) then
  begin
    FOffsetFrom := (Source as TvTimeZoneOffset).OffsetFrom;
    FOffsetTo := (Source as TvTimeZoneOffset).OffsetTo;
    FDTStart := (Source as TvTimeZoneOffset).DTStart;
    FName := (Source as TvTimeZoneOffset).Name;
  end;
end;

{ TvTimeZone }

function TvTimeZone.ToXml: String;
var
  doc: TXmlDocument;
  e0, e2: IXmlNode;
begin
  doc := TXmlDocument.Create(Self.FOwner);
  try
    doc.Active := true;
    e0 := doc.AddChild('vcardcollection');
    e2 := e0.AddChild('timezone');
    e2.AddChild('id').Text := FID;
    e2.AddChild('dloffsetfrom').Text := IntTostr(Self.FDayLight.FOffsetFrom);
    e2.AddChild('dldtstart').Text := DateTimeToIso(FDayLight.DTStart);
    e2.AddChild('dloffsetto').Text := IntTostr(Self.FDayLight.FOffsetTo);
    e2.AddChild('dlname').Text := Self.FDayLight.FName;
    e2.AddChild('url').Text := Self.FURL;
    e2.AddChild('soffsetfrom').Text := IntToStr(Self.FStandard.FOffsetFrom);
    e2.AddChild('sdtstart').Text := DateTimeToIso(Self.FStandard.FDTStart);
    e2.AddChild('soffsetto').Text := IntToStr(Self.FStandard.FOffsetTo);
    e2.AddChild('sname').Text := Self.FStandard.FName;
  finally
    Result := doc.XML.Text;
    doc.Free;
  end;
end;

procedure TvTimeZone.LoadFromXml(XMLData: String);
var
  doc: TXmlDocument;
  i: Integer;
  K: Integer;
  C: Integer;
begin
  doc := TXmlDocument.Create(FOwner.FvEvents.FOwner);
  C := 0;
  try
    doc.LoadFromXml(XmlData);
    doc.Active := true;
    for i := 0 to doc.DocumentElement.ChildNodes.Count - 1 do
    begin
      if doc.DocumentElement.ChildNodes[i].LocalName = 'vtimezone' then
      begin
        for K := 0 to doc.DocumentElement.ChildNodes[i].ChildNodes.Count - 1 do
        begin
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K].LocalName = 'id' then
                  Id := doc.DocumentElement.ChildNodes[i].ChildNodes[K].ChildNodes[C].Text;
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K].LocalName = 'dloffsetfrom' then
                  self.FDayLight.FOffsetFrom := StrToInt(doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text);
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'dldtstart' then
                  self.FDayLight.FDTStart := IsoToDateTime(doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text);
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'dloffsetto' then
                  self.FDayLight.FOffsetTo := StrToInt(doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text);
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'dlname' then
                  self.FDayLight.FName := doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text;
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'dlurl' then
                  self.URL := doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text;
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'soffsetfrom' then
                  self.Standard.FOffsetFrom := StrToInt(doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text);
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'sdtstart' then
                  self.FStandard.FDTStart := IsoToDateTime(doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text);
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'soffsetto' then
                  self.FStandard.FOffsetTo := StrToInt(doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text);
              if doc.DocumentElement.ChildNodes[i].ChildNodes[K]
                .LocalName = 'sname' then
                  self.FStandard.FName := doc.DocumentElement.ChildNodes[i]
                  .ChildNodes[K].ChildNodes[C].Text;
        end;
      end;
    end;
  finally
    doc.Free;
  end;
end;

procedure TvTimeZone.Assign(Source: TPersistent);
begin
  if (Source is TvTimeZone) then
  begin
    FID := (Source as TvTimeZone).ID;
    FURL := (Source as TvTimeZone).URL;
    FStandard.Assign((Source as TvTimeZone).Standard);
    FDaylight.Assign((Source as TvTimeZone).Daylight);
  end;
end;

constructor TvTimeZone.Create(Owner: TvCalendar);
begin
  inherited Create;
  FStandard := TvTimeZoneOffset.Create;
  FDayLight := TvTimeZoneOffset.Create;
  FOwner := Owner;
end;

destructor TvTimeZone.Destroy;
begin
  FStandard.Free;
  FDayLight.Free;
  inherited;
end;

procedure TvTimeZone.SetDaylight(const Value: TvTimeZoneOffset);
begin
  FDayLight := Value;
end;

procedure TvTimeZone.SetStandard(const Value: TvTimeZoneOffset);
begin
  FStandard := Value;
end;

end.
