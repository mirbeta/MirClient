{*******************************************************}
{                MiTeC Common Routines                  }
{                 Command Line Parser                   }
{                                                       }
{          Copyright (c) 2009-2017 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_CmdLine;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
    TSwitch = record
    Switch,
    Parameter: string;
    Parameters: TStringList;
    Present: Boolean;
  end;

  TSwitches = array of TSwitch;

  TCommandLine = class
  private
    FSwitches: TSwitches;
    FSL: TStrings;
    function GetSwitch(AIndex: integer): TSwitch;
    function GetSwitchCount: integer;
    function GetSwitchByName(const AName: string): TSwitch;
  public
    constructor Create; overload;
    constructor Create(ACmdLine: string); overload;

    destructor Destroy; override;

    procedure Clear;

    function FindSwitch(const ASwitch: string): Integer;
    function FindAtLeastOne(const ASwitches: array of string): Integer;

    property Switches[AIndex: integer]: TSwitch read GetSwitch;
    property SwitchCount: integer read GetSwitchCount;
    property Switch[const AName: string]: TSwitch read GetSwitchByName;

    property ParamList: TStrings read FSL;
  end;

var
  CmdLine: TCommandLine;

implementation

uses MiTeC_StrUtils;

function DequoteStr(Source: string; Quote: Char = '"'): string;
begin
  Result:=Source;
  if Length(Source)>1 then
    if (Source[1]=Quote) and (Source[Length(Source)]=Quote) then
      Result:=Copy(Source,2,Length(Source)-2);
end;

{ TCommandLine }

procedure TCommandLine.Clear;
var
  i: Integer;
begin
  for i:=0 to High(FSwitches) do
    if Assigned(FSwitches[i].Parameters) then
      FSwitches[i].Parameters.Free;
  Finalize(FSwitches);
end;

constructor TCommandLine.Create;
var
  i: Integer;
  idx: Integer;
  s,p: string;
begin
  FSL:=TStringList.Create;
  for i:=1 to ParamCount do begin
    s:=ParamStr(i);
    FSL.Add(s);
    {$IFDEF RAD7PLUS}
    if CharInSet(s[1],['/','-']) then begin
    {$else}
    if (s[1] in['/','-']) then begin
    {$ENDIF}
      p:='';
      SetLength(FSwitches,Length(FSwitches)+1);
      idx:=Pos(':',s);
      if idx>0 then begin
        p:=DequoteStr(Copy(s,idx+1,MAX_PATH),'''');
        s:=Copy(s,2,idx-2);
      end else
        s:=Copy(s,2,MAX_PATH);
      with FSwitches[High(FSwitches)] do begin
        Present:=True;
        Switch:=s;
        Parameter:=p;
        Parameters:=TStringList.Create;
        {$IFDEF RAD5PLUS}
        Parameters.Delimiter:=';';
        Parameters.StrictDelimiter:=True;
        if Pos(';',p)>0 then
          Parameters.DelimitedText:=p;
        {$ELSE}
        if Pos(';',p)>0 then
          SetDelimitedText(p,';',Parameters);
        {$ENDIF}
      end;
    end;
  end;
end;

constructor TCommandLine.Create(ACmdLine: string);
var
  i: Integer;
  idx: Integer;
  s,p: string;
begin
  FSL:=TStringList.Create;
  FSL.Text:=ACmdLine;
  for i:=0 to FSL.Count-1 do begin
    s:=FSL[i];
    {$IFDEF RAD7PLUS}
    if CharInSet(s[1],['/','-']) then begin
    {$else}
    if (s[1] in['/','-']) then begin
    {$ENDIF}
      p:='';
      SetLength(FSwitches,Length(FSwitches)+1);
      idx:=Pos(':',s);
      if idx>0 then begin
        p:=DequoteStr(Copy(s,idx+1,MAX_PATH),'''');
        s:=Copy(s,2,idx-2);
      end else
        s:=Copy(s,2,MAX_PATH);
      with FSwitches[High(FSwitches)] do begin
        Present:=True;
        Switch:=s;
        Parameter:=p;
        Parameters:=TStringList.Create;
        {$IFDEF RAD5PLUS}
        Parameters.Delimiter:=';';
        Parameters.StrictDelimiter:=True;
        if Pos(';',p)>0 then
          Parameters.DelimitedText:=p;
        {$ELSE}
        if Pos(';',p)>0 then
          SetDelimitedText(p,';',Parameters);
        {$ENDIF}
      end;
    end;
  end;
end;

destructor TCommandLine.Destroy;
begin
  FSL.Free;
  Clear;
  inherited;
end;

function TCommandLine.FindAtLeastOne(const ASwitches: array of string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(ASwitches) do begin
    Result:=FindSwitch(ASwitches[i]);
    if Result>-1 then
      Break;
  end;
end;

function TCommandLine.FindSwitch(const ASwitch: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FSwitches) do
    if SameText(ASwitch,FSwitches[i].Switch) then begin
      Result:=i;
      Break;
    end;
end;

function TCommandLine.GetSwitch(AIndex: integer): TSwitch;
begin
  Zeromemory(@Result,SizeOf(Result));
  if AIndex>0 then
    try
      Result:=FSwitches[AIndex];
      Result.Present:=True;
    except
    end;
end;

function TCommandLine.GetSwitchByName(const AName: string): TSwitch;
begin
  Result:=GetSwitch(FindSwitch(AName));
end;

function TCommandLine.GetSwitchCount: integer;
begin
  Result:=Length(FSwitches);
end;

initialization
  CmdLine:=TCommandLine.Create;
finalization
  CmdLine.Free;
end.

