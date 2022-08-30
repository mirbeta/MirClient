{*******************************************************}
{                MiTeC Common Routines                  }
{                 PrefsJS file parser                   }
{                                                       }
{          Copyright (c) 2009-2013 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_PrefsJs;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  TUserPref = record
    Name: string;
    Value: variant;
  end;

  TUserPrefs = array of TUserPref;

  TPrefsJS = class
  private
    FUserPrefs: TUserPrefs;
    FFilename: string;
    function GetUserPref(Index: Integer): TUserPref;
    procedure SetFilename(const Value: string);
    function GetUserPrefCount: Cardinal;
    function GetUserPrefByName(Name: string): TUserPref;
  public
    constructor Create;
    destructor Destroy; override;

    property FileName: string read FFilename write SetFilename;
    property UserPrefs[Index: Integer]: TUserPref read GetUserPref;
    property UserPrefsByName[Name: string]: TUserPref read GetUserPrefByName;
    property UserPrefCount: Cardinal read GetUserPrefCount;
  end;

implementation

uses MiTeC_StrUtils;

{ TPrefsJS }

constructor TPrefsJS.Create;
begin

end;

destructor TPrefsJS.Destroy;
begin
  Finalize(FUserPrefs);
  inherited;
end;

function TPrefsJS.GetUserPref(Index: Integer): TUserPref;
begin
  Result:=FUserPrefs[Index];
end;

function TPrefsJS.GetUserPrefByName(Name: string): TUserPref;
var
  i: Integer;
begin
  Result.Name:='';
  Result.Value:='';
  for i:=0 to High(FUserPrefs) do
    if SameText(Name,FUserPrefs[i].Name) then begin
      Result:=FUserPrefs[i];
      Break;
    end;
end;

function TPrefsJS.GetUserPrefCount: Cardinal;
begin
  Result:=Length(FUserPrefs);
end;

procedure TPrefsJS.SetFilename(const Value: string);
const
  up_kw = 'user_pref(';
var
  sl: TStringList;
  i,p,l: Integer;
  up: TUserPref;
  s: string;
begin
  l:=Length(up_kw);
  FFilename:=Value;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(FFilename);
    for i:=0 to sl.Count-1 do begin
      if PosText(up_kw,Trim(sl[i]))<>1 then
        Continue;
      p:=Pos(',',sl[i]);
      up.Name:=DeQuoteStr(Copy(sl[i],l+1,p-l-1));
      s:=Trim(Copy(sl[i],p+1,1024));
      SetLength(s,Length(s)-2);
      up.Value:=DeQuoteStr(s);

      SetLength(FUserPrefs,Length(FUserPrefs)+1);
      FUserPrefs[High(FUserPrefs)]:=up;
    end;
  finally
    sl.Free;
  end;
end;

end.
