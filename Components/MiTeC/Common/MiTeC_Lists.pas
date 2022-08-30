{*******************************************************}
{                    MiTeC Lists                        }
{                                                       }
{          Copyright (c) 1997-2016 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Lists;

interface

uses {$IFDEF RAD9PLUS}
     System.Variants, System.SysUtils, System.Classes, WinAPI.Windows, System.SyncObjs;
     {$ELSE}
     Variants, SysUtils, Classes, Windows, SyncObjs;
     {$ENDIF}

type
  TThreadStringList = class
  private
    FLock: TCriticalSection;
    FData: TStringList;
    function GetCommaText: string;
    function GetText: string;
    procedure SetCommaText(const Value: string);
    procedure SetText(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    procedure Add(const AValue: string);
    procedure Delete(AIndex: Integer);
    procedure DeleteLast;
    procedure Clear;
    function Get(AIndex: Integer): string;
    function GetLast: string;
    function GetName(AIndex: Integer): string;
    function GetValue(AIndex: Integer): string; overload;
    function GetValue(AName: string): string; overload;
    function Count: Integer;
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFilename: string);
    procedure LoadFromDelimitedString(AValue: string; ADelimiter: char);
    procedure LoadFromStrings(AValue: TStrings);
    procedure AddStrings(AValue: TStrings);

    property Text: string read GetText write SetText;
    property CommaText: string read GetCommaText write SetCommaText;
    property StringList: TStringList read FData;
  end;

  {$IFDEF D7PLUS}
  TValueRecord = record
    AsString: string;
    AsDouble: Double;
    AsDateTime: TDateTime;
    AsInteger: Integer;
    AsNumberString: string;
  end;

  TValueList = class(TStringList)
  private
    function GetValueAsDate(Name: string): TDatetime;
    function GetValueAsDateTime(Name: string): TDatetime;
    function GetValueAsDouble(Name: string): double;
    function GetValueAsInt(Name: string): integer;
    function GetValueAsInt64(Name: string): int64;
    function GetValueAsString(Name: string): string;
    procedure SetValueAsDate(Name: string; const Value: TDatetime);
    procedure SetValueAsDateTime(Name: string; const Value: TDatetime);
    procedure SetValueAsDouble(Name: string; const Value: double);
    procedure SetValueAsInt(Name: string; const Value: integer);
    procedure SetValueAsInt64(Name: string; const Value: int64);
    procedure SetValueAsString(Name: string; const Value: string);
    function GetValue(AName: string): TValueRecord;
  public
    constructor Create;

    property Val[AName: string]: TValueRecord read GetValue;
    property ValueAsString[Name: string]: string read GetValueAsString write SetValueAsString;
    property ValueAsInt[Name: string]: integer read GetValueAsInt write SetValueAsInt;
    property ValueAsInt64[Name: string]: int64 read GetValueAsInt64 write SetValueAsInt64;
    property ValueAsDouble[Name: string]: double read GetValueAsDouble write SetValueAsDouble;
    property ValueAsDate[Name: string]: TDatetime read GetValueAsDate write SetValueAsDate;
    property ValueAsDateTime[Name: string]: TDatetime read GetValueAsDateTime write SetValueAsDateTime;

    function ValueExists(AName: string): boolean;
  end;
  {$ENDIF}

  TNamedIntegerList = class(TStringList)
  protected
    function GetValue(AIndex: Integer): Integer;
    procedure PutValue(AIndex: Integer; AValue: Integer);
  public
    function Add(const AName: string; AValue: Integer): Integer; reintroduce;
    function IndexOfInt(AValue: Integer): Integer;
    procedure Insert(AIndex: Integer; const AName: string; AValue: Integer); reintroduce;
    property IntValues[AIndex: Integer]: Integer read GetValue write PutValue; default;
  end;

  TNamedFloatList = class(TStringList)
  private
    FDF: string;
  protected
    function GetValue(AIndex: Integer): double;
    procedure PutValue(AIndex: Integer; AValue: double);
  public
    constructor Create(ADoubleFormat: string = '%1.5f');
    function Add(const AName: string; AValue: double): Integer; reintroduce;
    function IndexOfFloat(AValue: double): Integer;
    procedure Insert(AIndex: Integer; const AName: string; AValue: double); reintroduce;
    property FloatValues[AIndex: Integer]: double read GetValue write PutValue; default;
    property DoubleFormat: string read FDF;
  end;

implementation

uses MiTeC_StrUtils;

function Quote(Source: string): string;
begin
  Result:=Source;
  if Copy(Source,1,1)='''' then
    Exit;
  Result:=''''+FastStringReplace(Source,'''','''''')+'''';
end;

function Dequote(Source: string): string;
begin
  Result:=Source;
  if Length(Source)>1 then begin
    if (Source[1]='''') and (Source[Length(Source)]='''') then
      Result:=Copy(Source,2,Length(Source)-2);
    Result:=FastStringReplace(Result,'''''','''');
  end;
end;

{$IFDEF D7PLUS}

{ TValueList }

constructor TValueList.Create;
begin
  inherited Create;
  NameValueSeparator:=#30;
end;

function TValueList.GetValue(AName: string): TValueRecord;
begin
  Result.AsString:=ValueAsString[AName];
  Result.AsDouble:=ValueAsDouble[AName];
  Result.AsDateTime:=ValueAsDatetime[AName];
  Result.AsInteger:=ValueAsInt[AName];
  Result.AsNumberString:=StringReplace(ValueAsString[AName],{$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}ThousandSeparator,'',[rfReplaceAll,rfIgnoreCase]);
  Result.AsNumberString:=StringReplace(Result.AsNumberString,{$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator,'.',[rfReplaceAll,rfIgnoreCase]);
end;

function TValueList.GetValueAsDate(Name: string): TDatetime;
begin
  Result:=StrToDateTimeDef(Dequote(Values[Name]),0);
end;

function TValueList.GetValueAsDateTime(Name: string): TDatetime;
begin
  Result:=StrToDateTimeDef(Dequote(Values[Name]),0);
end;

function TValueList.GetValueAsDouble(Name: string): double;
begin
  Result:=StrToFloatDef(TrimAll(Dequote(Values[Name])),0);
end;

function TValueList.GetValueAsInt(Name: string): integer;
begin
  Result:=Round(StrToFloatDef(TrimAll(Dequote(Values[Name])),0));
end;

function TValueList.GetValueAsInt64(Name: string): int64;
begin
  Result:=Round(StrToFloatDef(TrimAll(Dequote(Values[Name])),0));
end;

function TValueList.GetValueAsString(Name: string): string;
var
  idx: Integer;
begin
  idx:=IndexOfName(Name)+1;
  Result:=Values[Name];
  while (idx<Count) and (Pos(NameValueSeparator,Strings[idx])=0) do begin
    Result:=Result+#13#10+Strings[idx];
    Inc(idx);
  end;
  Result:=Dequote(Result);
end;

procedure TValueList.SetValueAsDate(Name: string; const Value: TDatetime);
begin
  if IndexOfName(Name)=-1 then
    Add(Format('%s%s%s',[Name,NameValueSeparator,Quote(DateToStr(Value))]))
  else
    Values[Name]:=Quote(DateToStr(Value));
end;

procedure TValueList.SetValueAsDateTime(Name: string; const Value: TDatetime);
begin
  if IndexOfName(Name)=-1 then
    Add(Format('%s%s%s',[Name,NameValueSeparator,Quote(DateTimeToStr(Value))]))
  else
    Values[Name]:=Quote(DateTimeToStr(Value));
end;

procedure TValueList.SetValueAsDouble(Name: string; const Value: double);
begin
  if IndexOfName(Name)=-1 then
    Add(Format('%s%s%s',[Name,NameValueSeparator,Quote(Format('%n',[Value]))]))
  else
    Values[Name]:=Quote(Format('%n',[Value]));
end;

procedure TValueList.SetValueAsInt(Name: string; const Value: integer);
begin
  if IndexOfName(Name)=-1 then
    Add(Format('%s%s%s',[Name,NameValueSeparator,Quote(Format('%1.0n',[Value/1]))]))
  else
    Values[Name]:=Quote(Format('%1.0n',[Value/1]));
end;

procedure TValueList.SetValueAsInt64(Name: string; const Value: int64);
begin
  if IndexOfName(Name)=-1 then
    Add(Format('%s%s%s',[Name,NameValueSeparator,Quote(Format('%1.0n',[Value/1]))]))
  else
    Values[Name]:=Quote(Format('%1.0n',[Value/1]));
end;

procedure TValueList.SetValueAsString(Name: string; const Value: string);
begin
  if IndexOfName(Name)=-1 then
    Add(Format('%s%s%s',[Name,NameValueSeparator,Quote(Value)]))
  else
    Values[Name]:=Quote(Value);
end;

function TValueList.ValueExists(AName: string): boolean;
begin
  Result:=IndexOfName(AName)>-1;
end;

{$ENDIF}

{ TNamedFloatList }

function TNamedFloatList.Add(const AName: string; AValue: double): Integer;
begin
  Result:=inherited Add(AName+'='+Format(FDF,[AValue]));
end;

constructor TNamedFloatList.Create;
begin
  inherited Create;
  FDF:=ADoubleFormat;
end;

function TNamedFloatList.GetValue(AIndex: Integer): double;
begin
  Result:=StrToFloat(Values[Names[AIndex]]);
end;

function TNamedFloatList.IndexOfFloat(AValue: double): Integer;
var
  P: Integer;
  s: string;
begin
  for Result:=0 to Count-1 do begin
    s:=Strings[Result];
    P:=AnsiPos('=',s);
    if (P<>0) and (AnsiCompareText(Trim(Copy(s,P+1,32)),Format(FDF,[AValue]))=0) then
      Exit;
  end;
  Result:=-1;
end;

procedure TNamedFloatList.Insert(AIndex: Integer; const AName: string;
  AValue: double);
begin
  inherited Insert(AIndex,AName+'='+Format(FDF,[AValue]));
end;

procedure TNamedFloatList.PutValue(AIndex: Integer; AValue: double);
begin
  inherited Values[Names[AIndex]]:=Format(FDF,[AValue]);
end;

{ TNamedIntegerList }

function TNamedIntegerList.Add(const AName: string;
  AValue: Integer): Integer;
begin
  Result:=inherited Add(AName+'='+IntToStr(AValue));
end;

function TNamedIntegerList.GetValue(AIndex: Integer): Integer;
begin
  Result:=StrToInt(Values[Names[AIndex]]);
end;

function TNamedIntegerList.IndexOfInt(AValue: Integer): Integer;
var
  P: Integer;
  s: string;
begin
  for Result:=0 to Count-1 do begin
    s:=Strings[Result];
    P:=AnsiPos('=',s);
    if (P<>0) and (AnsiCompareText(Trim(Copy(s,P+1,32)),IntToStr(AValue))=0) then
      Exit;
  end;
  Result:=-1;
end;

procedure TNamedIntegerList.Insert(AIndex: Integer; const AName: string;
  AValue: Integer);
begin
  inherited Insert(AIndex,AName+'='+IntToStr(AValue));
end;

procedure TNamedIntegerList.PutValue(AIndex, AValue: Integer);
begin
  inherited Values[Names[AIndex]]:=IntToStr(AValue);
end;

{ TThreadStringList }

procedure TThreadStringList.Add(const AValue: string);
begin
  Lock;
  try
    FData.Add(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.AddStrings(AValue: TStrings);
begin
  Lock;
  try
    FData.AddStrings(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.Clear;
begin
  Lock;
  try
    FData.Clear;
  finally
    Unlock;
  end;
end;

function TThreadStringList.Count: Integer;
begin
  Lock;
  try
    Result:=FData.Count;
  finally
    Unlock;
  end;
end;

constructor TThreadStringList.Create;
begin
  FLock:=TCriticalSection.Create;
  FData:=TStringList.Create;
end;

procedure TThreadStringList.Delete(AIndex: Integer);
begin
  Lock;
  try
    FData.Delete(AIndex);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.DeleteLast;
begin
  Lock;
  try
    if FData.Count>0 then
      FData.Delete(FData.Count-1);
  finally
    Unlock;
  end;
end;

destructor TThreadStringList.Destroy;
begin
  FData.Free;
  FLock.Free;
  inherited;
end;

function TThreadStringList.Get(AIndex: Integer): string;
begin
  Lock;
  try
    Result:=FData[AIndex];
  finally
    Unlock;
  end;
end;

function TThreadStringList.GetCommaText: string;
begin
  Lock;
  try
    Result:=FData.CommaText;
  finally
    Unlock;
  end;
end;

function TThreadStringList.GetLast: string;
begin
  Result:='';
  Lock;
  try
    if FData.Count>0 then
      Result:=FData[FData.Count-1];
  finally
    Unlock;
  end;
end;

function TThreadStringList.GetName(AIndex: Integer): string;
begin
  Lock;
  try
    Result:=FData.Names[AIndex];
  finally
    Unlock;
  end;
end;

function TThreadStringList.GetText: string;
begin
  Lock;
  try
    Result:=FData.Text;
  finally
    Unlock;
  end;
end;

function TThreadStringList.GetValue(AIndex: Integer): string;
begin
  Lock;
  try
    Result:=FData.ValueFromIndex[AIndex];
  finally
    Unlock;
  end;
end;

function TThreadStringList.GetValue(AName: string): string;
begin
  Lock;
  try
    Result:=FData.Values[AName];
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.LoadFromDelimitedString(AValue: string;
  ADelimiter: char);
begin
  Lock;
  try
    FData.Delimiter:=ADelimiter;
    FData.StrictDelimiter:=True;
    FData.DelimitedText:=AValue;
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.LoadFromFile(const AFilename: string);
begin
  Lock;
  try
    FData.LoadFromFile(AFilename);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.LoadFromStream(AStream: TStream);
begin
  Lock;
  try
    FData.LoadFromStream(AStream);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.LoadFromStrings(AValue: TStrings);
begin
  Lock;
  try
    FData.Text:=AValue.Text;
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.Lock;
begin
  FLock.Enter;
end;

procedure TThreadStringList.SaveToFile(const AFilename: string);
begin
  Lock;
  try
    FData.SaveToFile(AFilename);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.SaveToStream(AStream: TStream);
begin
  Lock;
  try
    FData.SaveToStream(AStream);
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.SetCommaText(const Value: string);
begin
  Lock;
  try
    FData.CommaText:=Value;
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.SetText(const Value: string);
begin
  Lock;
  try
    FData.Text:=Value;
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.Unlock;
begin
  FLock.Leave;
end;

end.
