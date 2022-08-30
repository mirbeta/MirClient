unit dxDemoTutorial;

interface

uses
  Types, Graphics, Generics.Defaults, Generics.Collections, Controls, Classes, cxRichEdit;

type
  { TTutorialEntryHeader }

  TTutorialEntryHeader = class
  strict private
    FCaption: string;
    function GetKey: string;
  public
    Tags: TStringDynArray;
    constructor Create(const ACaption: string);
    property Caption: string read FCaption;
    property Key: string read GetKey;
  end;

  { TTutorialEntry }

  TTutorialEntry = class
  strict private
    FHeader: TTutorialEntryHeader;
    FSources: TcxRichEdit;
    function GetCaption: string;
    function GetKey: string;
    function GetTags: TStringDynArray;
  protected
    Calculated: Boolean;
  public
    constructor Create(AHeader: TTutorialEntryHeader);
    destructor Destroy; override;

    property Caption: string read GetCaption;
    property Key: string read GetKey;
    property Sources: TcxRichEdit read FSources;
    property Tags: TStringDynArray read GetTags;
  end;

  { TTutorialInfo }

  TTutorialInfo = class
  strict private
    FDescription: string;
    FEntries: TList<TTutorialEntry>;
  public
    constructor Create(const ADescription: string);
    destructor Destroy; override;

    property Description: string read FDescription;
    property Entries: TList<TTutorialEntry> read FEntries;
  end;

  { TTutorialManager }

  TTutorialManager = class
  strict private const
    DefaultEntryCaption = 'Code';
  strict private type
     TCanHighlightFunc = function(const AText: string): Boolean;
     THighlightInfo = record
       CanHighlightFunc: TCanHighlightFunc;
       Color: TColor;
       FontStyles: TFontStyles;
       NeedHighlight: Boolean;
       Keyword: string;
     end;
  strict private
    FTutorialEntryCache: TObjectDictionary<string, TTutorialEntry>;
    FTutorials: TObjectDictionary<TWinControl, TTutorialInfo>;

    function CreateHeaders(const ATags: TStringDynArray): TList<TTutorialEntryHeader>;
    function GetSourceCodeByTag(const ATags, AUnits: TStringDynArray): string;
    procedure PopulateInfo(AControl: TWinControl; AInfo: TTutorialInfo; const ATags, AUnits: TStringDynArray);
    class function CanHighlightDelphiAttributes(const AText: string): Boolean; static;
    class function CanHighlightDelphiComments(const AText: string): Boolean; static;
    class function CanHighlightDelphiDefines(const AText: string): Boolean; static;
    class function CanHighlightDelphiKeywords(const AText: string): Boolean; static;
    class procedure HighlightDelphiAttributes(ARichEdit: TcxRichEdit);
    class procedure HighlightDelphiComments(ARichEdit: TcxRichEdit);
    class procedure HighlightDelphiDefines(ARichEdit: TcxRichEdit);
    class procedure HighlightDelphiKeywords(ARichEdit: TcxRichEdit);
    class procedure HihglightKeyword(ARichEdit: TcxRichEdit; const AInfo: THighlightInfo);
  public
    constructor Create;
    destructor Destroy; override;

    function TryGetValue(AControl: TWinControl; out AValue: TTutorialInfo): Boolean;
    procedure RegisterTutorial(AControl: TWinControl; const ADescription: string; const ATags, AUnits: array of string);
    procedure UnregisterTutorial(AControl: TWinControl);
  end;

function TutorialManager: TTutorialManager;

implementation

uses
  Messages, SysUtils, ComCtrls, dxCore;

const
  dxSourceCodeLoadingError = 'Could not load the %s unit';

var
  dxgTutorialManager: TTutorialManager;

function TutorialManager: TTutorialManager;
begin
  if dxgTutorialManager = nil then
    dxgTutorialManager := TTutorialManager.Create;
  Result := dxgTutorialManager;
end;

{ TTutorialEntryHeader }

constructor TTutorialEntryHeader.Create(const ACaption: string);
begin
  inherited Create;
  FCaption := ACaption;
end;

function TTutorialEntryHeader.GetKey: string;
var
  I: Integer;
begin
  Result := Caption;
  for I := 0 to Length(Tags) - 1 do
    Result := Result + ',' + Tags[I];
end;

{ TTutorialEntry }

constructor TTutorialEntry.Create(AHeader: TTutorialEntryHeader);
begin
  inherited Create;
  FHeader := AHeader;
  FSources := TcxRichEdit.Create(nil);
  FSources.Properties.WordWrap := False;
  FSources.Visible := False;
end;

destructor TTutorialEntry.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FSources);
  inherited Destroy;
end;

function TTutorialEntry.GetCaption: string;
begin
  Result := FHeader.Caption;
end;

function TTutorialEntry.GetKey: string;
begin
  Result := FHeader.Key;
end;

function TTutorialEntry.GetTags: TStringDynArray;
begin
  Result := FHeader.Tags;
end;

{ TTutorialInfo }

constructor TTutorialInfo.Create(const ADescription: string);
begin
  inherited Create;
  FDescription := ADescription;
  FEntries := TList<TTutorialEntry>.Create;
end;

destructor TTutorialInfo.Destroy;
begin
  FreeAndNil(FEntries);
  inherited Destroy;
end;

{ TTutorialManager }

constructor TTutorialManager.Create;
begin
  inherited Create;
  FTutorials := TObjectDictionary<TWinControl, TTutorialInfo>.Create([doOwnsValues]);
  FTutorialEntryCache := TObjectDictionary<string, TTutorialEntry>.Create([doOwnsValues]);
end;

destructor TTutorialManager.Destroy;
begin
  FreeAndNil(FTutorialEntryCache);
  FreeAndNil(FTutorials);
  inherited Destroy;
end;

class function TTutorialManager.CanHighlightDelphiAttributes(const AText: string): Boolean;
begin
  Result := (Pos('[', AText) = 1) and (Pos(']', AText) = Length(AText));
end;

class function TTutorialManager.CanHighlightDelphiComments(const AText: string): Boolean;
begin
  Result := Pos('//', AText) = 1;
end;

class function TTutorialManager.CanHighlightDelphiDefines(const AText: string): Boolean;
begin
  Result := Pos('{$', AText) = 1;
end;

class function TTutorialManager.CanHighlightDelphiKeywords(const AText: string): Boolean;
begin
  Result := Pos('''', AText) <= 0;
end;

class procedure TTutorialManager.HighlightDelphiAttributes(ARichEdit: TcxRichEdit);

  procedure DoHighlight(const AKeyword: string);
  var
    AInfo: THighlightInfo;
  begin
    AInfo.Color := clMaroon;
    AInfo.NeedHighlight := False;
    AInfo.CanHighlightFunc := @CanHighlightDelphiAttributes;
    AInfo.Keyword := AKeyword;
    AInfo.FontStyles := [fsBold];
    HihglightKeyword(ARichEdit, AInfo);
  end;

begin
  ARichEdit.Lines.BeginUpdate;
  try
    DoHighlight('Aggregated');
    DoHighlight('Association');
    DoHighlight('Automapping');
    DoHighlight('Column');
    DoHighlight('Entity');
    DoHighlight('Generator');
    DoHighlight('Indexed');
    DoHighlight('Key');
    DoHighlight('Nullable');
    DoHighlight('Size');
    DoHighlight('Table');
  finally
    ARichEdit.Lines.EndUpdate;
  end;
end;

class procedure TTutorialManager.HighlightDelphiComments(ARichEdit: TcxRichEdit);
var
  AInfo: THighlightInfo;
begin
  AInfo.Color := clGreen;
  AInfo.NeedHighlight := True;
  AInfo.CanHighlightFunc := @CanHighlightDelphiComments;
  AInfo.Keyword := '//';
  AInfo.FontStyles := [fsBold];
  ARichEdit.Lines.BeginUpdate;
  try
    HihglightKeyword(ARichEdit, AInfo);
  finally
    ARichEdit.Lines.EndUpdate;
  end;
end;

class procedure TTutorialManager.HighlightDelphiDefines(ARichEdit: TcxRichEdit);
var
  AInfo: THighlightInfo;
begin
  AInfo.Color := clGreen;
  AInfo.NeedHighlight := True;
  AInfo.CanHighlightFunc := @CanHighlightDelphiDefines;
  AInfo.Keyword := '{';
  AInfo.FontStyles := [fsBold];
  ARichEdit.Lines.BeginUpdate;
  try
    HihglightKeyword(ARichEdit, AInfo);
  finally
    ARichEdit.Lines.EndUpdate;
  end;
end;

class procedure TTutorialManager.HighlightDelphiKeywords(ARichEdit: TcxRichEdit);

  procedure DoHighlight(const AKeyword: string);
  var
    AInfo: THighlightInfo;
  begin
    AInfo.Color := clBlue;
    AInfo.NeedHighlight := False;
    AInfo.CanHighlightFunc := @CanHighlightDelphiKeywords;
    AInfo.Keyword := AKeyword;
    AInfo.FontStyles := [];
    HihglightKeyword(ARichEdit, AInfo);
  end;

begin
  ARichEdit.Lines.BeginUpdate;
  try
    DoHighlight('procedure');
    DoHighlight('function');
    DoHighlight('begin');
    DoHighlight('end');
    DoHighlight('var');
    DoHighlight('if');
    DoHighlight('else');
    DoHighlight('then');
    DoHighlight('for');
    DoHighlight('to');
    DoHighlight('do');
    DoHighlight('in');
    DoHighlight('not');
    DoHighlight('and');
    DoHighlight('or');
    DoHighlight('strict private');
    DoHighlight('private');
    DoHighlight('public');
    DoHighlight('property');
    DoHighlight('class');
    DoHighlight('read');
    DoHighlight('write');
    DoHighlight('constructor');
    DoHighlight('destructor');
    DoHighlight('interface');
    DoHighlight('inherited');
    DoHighlight('override');
    DoHighlight('try');
    DoHighlight('except');
    DoHighlight('finally');
  finally
    ARichEdit.Lines.EndUpdate;
  end;
end;

function TTutorialManager.TryGetValue(AControl: TWinControl; out AValue: TTutorialInfo): Boolean;
begin
  Result := FTutorials.TryGetValue(AControl, AValue);
end;

procedure TTutorialManager.RegisterTutorial(AControl: TWinControl; const ADescription: string;
  const ATags, AUnits: array of string);

  function ToStringDynArray(AArray: array of string): TStringDynArray;
  var
    I: Integer;
  begin
    SetLength(Result, Length(AArray));
    for I := Low(AArray) to High(AArray) do
      Result[I] := AArray[I];
  end;

var
  AInfo: TTutorialInfo;
begin
  if AControl <> nil then
  begin
    AInfo := TTutorialInfo.Create(ADescription);
    PopulateInfo(AControl, AInfo, ToStringDynArray(ATags), ToStringDynArray(AUnits));
    FTutorials.Add(AControl, AInfo);
  end;
end;

procedure TTutorialManager.UnregisterTutorial(AControl: TWinControl);
var
  AEntry: TTutorialEntry;
  AInfo: TTutorialInfo;
  ARemovingEntries: TList<TTutorialEntry>;
begin
  if FTutorials.TryGetValue(AControl, AInfo) then
  begin
    ARemovingEntries := TList<TTutorialEntry>.Create;
    try
      for AEntry in AInfo.Entries do
        if FTutorialEntryCache.ContainsKey(AEntry.Caption) then
          ARemovingEntries.Add(AEntry);
      for AEntry in ARemovingEntries do
        AInfo.Entries.Extract(AEntry);
      FTutorials.Remove(AControl);
    finally
      ARemovingEntries.Free;
    end;
  end;
end;

function TTutorialManager.CreateHeaders(const ATags: TStringDynArray): TList<TTutorialEntryHeader>;

  procedure AddTags(const ATag: string; AHeader: TTutorialEntryHeader);
  var
    L: Integer;
  begin
    L := Length(AHeader.Tags);
    SetLength(AHeader.Tags, L + 1);
    AHeader.Tags[L] := ATag;
  end;

  function FindEntryHeader(AEntriesHeaders: TList<TTutorialEntryHeader>; const ACaption: string;
    out AHeader: TTutorialEntryHeader): Boolean;
  var
    H: TTutorialEntryHeader;
  begin
    AHeader := nil;
    Result := False;
    for H in AEntriesHeaders do
    begin
      Result := H.Caption = ACaption;
      if Result then
        AHeader := H;
    end;
  end;

var
  ACaption, ATag, ACurrentTag: string;
  APosition: Integer;
  AHeader: TTutorialEntryHeader;
begin
  Result := TList<TTutorialEntryHeader>.Create;
  for ATag in ATags do
  begin
    ACurrentTag := ATag;
    APosition := Pos('|', ATag);
    if APosition > 0 then
    begin
      ACaption := Copy(ATag, 1, APosition - 1);
      ACurrentTag := Copy(ATag, APosition + 1, MaxInt);
    end
    else
      ACaption := DefaultEntryCaption;
    if not FindEntryHeader(Result, ACaption, AHeader) then
    begin
      AHeader := TTutorialEntryHeader.Create(ACaption);
      Result.Add(AHeader);
    end;
    AddTags(ACurrentTag, AHeader);
  end;
end;

function TTutorialManager.GetSourceCodeByTag(const ATags, AUnits: TStringDynArray): string;

  procedure ProcessTag(const AUnitName, ATag: string; ASources: TStringList);

    function IsTutorialString(const S: string): Boolean;
    begin
      Result := (Pos('//<', S) > 0) or (Pos('//>', S) > 0);
    end;

  var
    I: Integer;
    AUnit: TStringList;
    APrevText: string;
    AStarted, AExcludeAllTag: Boolean;
  begin
    APrevText := ASources.Text;
    AStarted := False;
    AExcludeAllTag := ATag = '*';
    AUnit := TStringList.Create;
    try
      AUnit.LoadFromFile(AUnitName);
      for I := 0 to AUnit.Count - 1 do
        if AExcludeAllTag then
        begin
          if IsTutorialString(AUnit[I]) then
            ASources.Add(AUnit[I]);
        end
        else
          if not AStarted then
            AStarted := Trim(AUnit[I]) = '//<' + ATag
          else
          begin
            if (Trim(AUnit[I]) = '//>' + ATag) then
              AStarted := False
            else
              if not IsTutorialString(AUnit[I]) then
                ASources.Add(AUnit[I]);
          end;
    finally
      AUnit.Free;
    end;
  end;

var
  I: Integer;
  ASources: TStringList;
  AUnit: string;
begin
  ASources := TStringList.Create;
  try
    for AUnit in AUnits do
      if FileExists(AUnit) then
        for I := Low(ATags) to High(ATags) do
          ProcessTag(AUnit, ATags[I], ASources)
      else
        ASources.Add(Format(dxSourceCodeLoadingError, [AUnit]));
    Result := Copy(ASources.Text, 1, Length(ASources.Text) - 2);
  finally
    ASources.Free;
  end;
end;

procedure TTutorialManager.PopulateInfo(AControl: TWinControl; AInfo: TTutorialInfo; const ATags, AUnits: TStringDynArray);
var
  AEntry: TTutorialEntry;
  AHeader: TTutorialEntryHeader;
  AHeaders: TList<TTutorialEntryHeader>;
begin
  AHeaders := CreateHeaders(ATags);
  try
    for AHeader in AHeaders do
    begin
      if not FTutorialEntryCache.TryGetValue(AHeader.Key, AEntry) then
      begin
        AEntry := TTutorialEntry.Create(AHeader);
        AEntry.Sources.Parent := AControl;
        AEntry.Sources.Text := GetSourceCodeByTag(AEntry.Tags, AUnits);
        HighlightDelphiAttributes(AEntry.Sources);
        HighlightDelphiKeywords(AEntry.Sources);
        HighlightDelphiComments(AEntry.Sources);
        HighlightDelphiDefines(AEntry.Sources);
        AEntry.Sources.Parent := nil;
        AEntry.Calculated := True;
        FTutorialEntryCache.Add(AEntry.Key, AEntry);
      end
      else
        AHeader.Free;
      AInfo.Entries.Add(AEntry);
    end;
  finally
    AHeaders.Free;
  end;
end;

class procedure TTutorialManager.HihglightKeyword(ARichEdit: TcxRichEdit; const AInfo: THighlightInfo);

  function CanHighlight(ARichEdit: TcxRichEdit; ACharPosition: Integer; out ALineIndex: Integer): Boolean;
  begin
    ALineIndex := ARichEdit.InnerControl.Perform(EM_LINEFROMCHAR, ACharPosition + 1, 0);
    Result := AInfo.CanHighlightFunc(TrimLeft(TrimRight(ARichEdit.Lines[ALineIndex])));
  end;

var
  AStart, APosition, ALineIndex, ALength: Integer;
begin
  AStart := 0;
  ARichEdit.Lines.BeginUpdate;
  try
    while ARichEdit.FindText(AInfo.Keyword, AStart, Length(ARichEdit.Text), [stMatchCase]) <> -1 do
    begin
      APosition := ARichEdit.FindText(AInfo.Keyword, AStart, Length(ARichEdit.Text) - AStart, [stMatchCase, stWholeWord]);
      ALength := Length(AInfo.Keyword);
      if CanHighlight(ARichEdit, APosition, ALineIndex) then
      begin
        if AInfo.NeedHighlight then
          ALength := Length(ARichEdit.Lines[ALineIndex]);
        ARichEdit.SelStart  := APosition;
        ARichEdit.SelLength := ALength;
        ARichEdit.SelAttributes.Color := AInfo.Color;
        ARichEdit.SelAttributes.Style := AInfo.FontStyles;
      end;
      Inc(AStart, ALength);
    end;
  finally
    ARichEdit.Lines.EndUpdate;
  end;
end;

initialization

finalization
  FreeAndNil(dxgTutorialManager);

end.


