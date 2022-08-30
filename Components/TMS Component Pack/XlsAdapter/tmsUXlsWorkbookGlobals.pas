unit tmsUXlsWorkbookGlobals;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords, tmsUXlsChart,
     tmsUXlsSST, tmsXlsMessages, tmsUXlsSections, tmsUXlsReferences, tmsUSheetNameList, tmsUXlsEscher,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     tmsUXlsFormula, tmsUEscherRecords, tmsUXlsPalette, tmsUXlsXF, tmsUFlxMessages, tmsUOle2Impl;
type
  TBoundSheetList = class
  private
   FSheetNames: TSheetNameList;  //Cache with all the sheet names to speed up searching
   FBoundSheets: TBoundSheetRecordList;
  public
    property BoundSheets: TBoundSheetRecordList read FBoundSheets;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Add(const aRecord: TBoundSheetRecord);

    procedure SaveToStream(const DataStream: TOle2File);
    procedure SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer);
    function TotalSize:int64;
    function TotalRangeSize(const SheetIndex: integer): int64;

    procedure InsertSheet(const BeforeSheet: integer; const OptionFlags: word; const SheetName: UTF16String);
    procedure DeleteSheet(const SheetIndex: integer);
  end;

  TWorkbookGlobals = class(TBaseSection)
  private
    FSST: TSST;
    FReferences: TReferences;
    FBoundSheets: TBoundSheetList;
    FMiscRecords: TBaseRecordList;
    FNames : TNameRecordList;
    FDrawingGroup: TDrawingGroup;
    FWindow1: TWindow1Record;

    F1904: T1904Record;
    FBookBool: TBookBoolRecord;
    FPrecision: TPrecisionRecord;

    FXF: TXFRecordList;
    FFonts: TFontRecordList;
    FFormats: TFormatRecordList;

    FPaletteCache: TPaletteRecord;
    FPaletteIndex: integer;

    FHasMacro: boolean;
    FIsXltTemplate: boolean;

    FCodeName: UTF16String;

    function GetSheetCount: integer;
    function GetSheetName(const index: integer): UTF16String;
    procedure SetSheetName(const index: integer; const Value: UTF16String);
    function GetSheetVisible(const index: integer): TXlsSheetVisible;
    procedure SetSheetVisible(const index: integer; const Value: TXlsSheetVisible);
    function GetSheetOptionFlags(const index: integer): word;
    function GetActivesheet: integer;
    procedure SetActiveSheet(const Value: integer);
    function GetColorPalette(Index: integer): LongWord;
    procedure SetColorPalette(Index: integer; const Value: LongWord);
    function GetIs1904: boolean;
    function GetPrecisionAsDisplayed: boolean;
    function GetSaveExternalLinkValues: boolean;
    procedure SetIs1904(const Value: boolean);
    procedure SetPrecisionAsDisplayed(const Value: boolean);
    procedure SetSaveExternalLinkValues(const Value: boolean);
  public
    property SST: TSST read FSST;

    property SheetName[const index: integer]: UTF16String read GetSheetName write SetSheetName;
    procedure SetFirstSheetVisible(const index: integer);
    property SheetVisible[const index: integer]: TXlsSheetVisible read GetSheetVisible write SetSheetVisible;
    property SheetCount: integer read GetSheetCount;
    property SheetOptionFlags[const index: integer]: word read GetSheetOptionFlags;
    procedure SheetSetOffset(const index: integer; const Offset: LongWord);

    property ActiveSheet: integer read GetActivesheet write SetActiveSheet;

    property DrawingGroup: TDrawingGroup read FDrawingGroup;
    property References: TReferences read FReferences;
    property Names: TNameRecordList read FNames;

    property HasMacro: boolean read FHasMacro;
    property CodeName: UTF16String read FCodeName;

    property IsXltTemplate: boolean read FIsXltTemplate write FIsXltTemplate;

    constructor Create;
    destructor Destroy; override;
    function TotalSize:int64; override;
    function TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange): int64; override;
    procedure Clear; override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBOFRecord; const SST: TSST); override;
    procedure SaveToStream(const DataStream: TOle2File);override;
    procedure SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer; const CellRange: TXlsCellRange);override;

    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo);
    procedure DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word;const SheetInfo: TSheetInfo);

    procedure DeleteSheets(const SheetIndex, SheetCount: integer);
    procedure InsertSheets(const CopyFrom: integer; BeforeSheet: integer; const OptionFlags: word; const Name: UTF16String; const SheetCount: integer);

    property ColorPalette[Index: integer]: LongWord read GetColorPalette write SetColorPalette;

    property XF:TXFRecordList read FXF;
    property Fonts:TFontRecordList read FFonts;
    property Formats:TFormatRecordList read FFormats;

    property Is1904: boolean read GetIs1904 write SetIs1904;
    property PrecisionAsDisplayed: boolean read GetPrecisionAsDisplayed write SetPrecisionAsDisplayed;
    property SaveExternalLinkValues: boolean read GetSaveExternalLinkValues write SetSaveExternalLinkValues;

    procedure DeleteCountry;
    class function IsValidRangeName(const Name: UTF16String; var OptionFlags: Integer): Boolean;
    procedure CheckInternalNames(const OptionFlags: integer);
    procedure AddName(var Range: TXlsNamedRange; const CellList: pointer);
    function  GetName(const sheet: Int32; const aName: UTF16String): TNameRecord;
  end;
implementation
{ TBoundSheetList }

procedure TBoundSheetList.Add(const aRecord: TBoundSheetRecord);
begin
  FSheetNames.Add(aRecord.SheetName);
  FBoundSheets.Add(aRecord); //Last
end;

procedure TBoundSheetList.Clear;
begin
  if FSheetNames<>nil then FSheetNames.Clear;
  if FBoundSheets<>nil then FBoundSheets.Clear;
end;

procedure TBoundSheetList.DeleteSheet(const SheetIndex: integer);
begin
  FSheetNames.DeleteSheet(FBoundSheets.SheetName[SheetIndex]);
  FBoundSheets.Delete(SheetIndex);
end;

constructor TBoundSheetList.Create;
begin
  inherited;
  FSheetNames:= TSheetNameList.Create;
  FBoundSheets:= TBoundSheetRecordList.Create;
end;

destructor TBoundSheetList.Destroy;
begin
  FreeAndNil(FSheetNames);
  FreeAndNil(FBoundSheets);
  inherited;
end;

procedure TBoundSheetList.InsertSheet(const BeforeSheet: integer;
  const OptionFlags: word; const SheetName: UTF16String);
var
  NewName: UTF16String;
begin
  NewName:= FSheetNames.AddUniqueName(SheetName);
  FBoundSheets.Insert(BeforeSheet, TBoundSheetRecord.CreateNew(OptionFlags, NewName));
end;

procedure TBoundSheetList.SaveRangeToStream(const DataStream: TOle2File; const SheetIndex: integer);
begin
  if (SheetIndex>=FBoundSheets.Count)or (SheetIndex<0) then raise Exception.CreateFmt(ErrInvalidSheetNo, [SheetIndex,0,FBoundSheets.Count-1]);
  FBoundSheets[SheetIndex].SaveToStream(DataStream);
end;

procedure TBoundSheetList.SaveToStream(const DataStream: TOle2File);
begin
  FBoundSheets.SaveToStream(DataStream);
end;

function TBoundSheetList.TotalSize: int64;
begin
  TotalSize:= FBoundSheets.TotalSize;
end;

function TBoundSheetList.TotalRangeSize(const SheetIndex: integer): int64;
begin
  if (SheetIndex>=FBoundSheets.Count)or (SheetIndex<0) then raise Exception.CreateFmt(ErrInvalidSheetNo, [SheetIndex,0,FBoundSheets.Count-1]);
  Result:=FBoundSheets[SheetIndex].TotalSize;
end;

{ TWorkbookGlobals }

procedure TWorkbookGlobals.Clear;
begin
  inherited;
  if FSST<>nil then FSST.Clear;
  if FReferences<>nil then FReferences.Clear;
  if FBoundSheets<>nil then FBoundSheets.Clear;
  if FMiscRecords<>nil then FMiscRecords.Clear;
  if FNames<>nil then FNames.Clear;
  if FDrawingGroup<>nil then FDrawingGroup.Clear;
  if FXF<>nil then FXF.Clear;
  if FFonts<>nil then FFonts.Clear;
  if FFormats<>nil then FFormats.Clear;
  FPaletteCache:=nil;
  FWindow1:=nil;
  F1904:=nil;
  FBookBool:=nil;
  FPrecision:=nil;
  FHasMacro:=false;
  FIsXltTemplate:=false;
  FCodeName:='';
end;

constructor TWorkbookGlobals.Create;
begin
  inherited;
  FSST:= TSST.Create;
  FReferences:= TReferences.Create;
  FBoundSheets:= TBoundSheetList.Create;
  FMiscRecords:= TBaseRecordList.Create;
  FNames:=TNameRecordList.Create;
  FDrawingGroup:= TDrawingGroup.Create;
  FXF:= TXFRecordList.Create;
  FFonts:= TFontRecordList.Create;
  FFormats:= TFormatRecordList.Create;
  FPaletteCache:=nil;
  FWindow1:=nil;
  F1904:=nil;
  FBookBool:=nil;
  FPrecision:=nil;

  FHasMacro:=false;
  FIsXltTemplate:=false;
  FCodeName:='';
end;

procedure TWorkbookGlobals.DeleteRowsAndCols(const aRow, aRowCount, aCol, aColCount: word; const SheetInfo: TSheetInfo);
begin
  FNames.ArrangeInsertRowsAndCols(aRow, -aRowCount, aCol, -aColCount, SheetInfo);
end;

procedure TWorkbookGlobals.DeleteSheets(const SheetIndex,
  SheetCount: integer);
var
  i: integer;
begin
  if HasMacro then raise Exception.Create(ErrCantDeleteSheetWithMacros);  //If we delete a sheet that has a corresponding macro on the vba stream, Excel 2000 will crash when opening the file. Excel Xp seems to handle this ok.
  for i:=0 to SheetCount-1 do
      FBoundSheets.DeleteSheet(SheetIndex);
  FReferences.InsertSheets(SheetIndex, -SheetCount);
  FNames.DeleteSheets(SheetIndex, SheetCount);
end;

destructor TWorkbookGlobals.Destroy;
begin
  FreeAndNil(FSST);
  FreeAndNil(FReferences);
  FreeAndNil(FBoundSheets);
  FreeAndNil(FMiscRecords);
  FreeAndNil(FNames);
  FreeAndNil(FDrawingGroup);
  FreeAndNil(FXF);
  FreeAndNil(FFonts);
  FreeAndNil(FFormats);
  inherited;
end;

function TWorkbookGlobals.GetActivesheet: integer;
begin
  if FWindow1<>nil then Result:= FWindow1.ActiveSheet else Result:=0;
end;

function TWorkbookGlobals.GetColorPalette(Index: integer): LongWord;
begin
  if FPaletteCache=nil then Result:=StandardPalette(Index) else Result:=FPaletteCache.Color[Index];
end;

function TWorkbookGlobals.GetIs1904: boolean;
begin
  if F1904<>nil then Result:=F1904.Is1904 else Result:=false;
end;

function TWorkbookGlobals.GetPrecisionAsDisplayed: boolean;
begin
  if FPrecision<>nil then Result:=FPrecision.PrecisionAsDisplayed else Result:=false;
end;

function TWorkbookGlobals.GetSaveExternalLinkValues: boolean;
begin
  if FBookBool<>nil then Result:=FBookBool.SaveExternalLinkValues else Result:=false;
end;

function TWorkbookGlobals.GetSheetCount: integer;
begin
  Result:= FBoundSheets.BoundSheets.Count;
end;

function TWorkbookGlobals.GetSheetName(const index: integer): UTF16String;
begin
  Result:= FBoundSheets.BoundSheets.SheetName[index];
end;

function TWorkbookGlobals.GetSheetOptionFlags(const index: integer): word;
begin
  Result:= FBoundSheets.BoundSheets[index].OptionFlags;
end;

function TWorkbookGlobals.GetSheetVisible(const index: integer): TXlsSheetVisible;
begin
  Result:= FBoundSheets.BoundSheets.SheetVisible[index];
end;

procedure TWorkbookGlobals.InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, aRowCount, FirstCol, LastCol, DestCol, aColCount: integer; const SheetInfo: TSheetInfo);
begin
  FNames.ArrangeInsertRowsAndCols(DestRow, (LastRow -FirstRow +1)* aRowCount, DestCol, (LastCol -FirstCol +1)* aColCount, SheetInfo);
end;

procedure TWorkbookGlobals.InsertSheets(const CopyFrom: integer; BeforeSheet: integer;
  const OptionFlags: word; const Name: UTF16String; const SheetCount: integer);
var
  i, ofs: integer;
  SheetInfo: TSheetInfo;
begin
  for i:=0 to SheetCount-1 do
    FBoundSheets.InsertSheet(BeforeSheet, OptionFlags, Name);
  FReferences.InsertSheets(BeforeSheet, SheetCount);

  SheetInfo.InsSheet:=-1;
  if CopyFrom>=BeforeSheet then ofs:=SheetCount else ofs:=0;
  SheetInfo.FormulaSheet:=CopyFrom + ofs;
  SheetInfo.GetSheet:= FReferences.GetSheet;
  SheetInfo.SetSheet:= FReferences.SetSheet;
  SheetInfo.Names:=nil;
  FNames.InsertSheets(CopyFrom, BeforeSheet, SheetCount, SheetInfo );
end;

procedure TWorkbookGlobals.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TBOFRecord; const SST: TSST);
var
  R: TBaseRecord;
  RecordId: integer;
begin
  Clear;
  repeat
    RecordId := RecordHeader.Id;
    R:=LoadRecords(DataStream, RecordHeader);
    try
      if (R is TXFRecord) and (FXF.Count=0) then FMiscRecords.Add(TSubListRecord.CreateAndAssign(FXF));
      if (R is TFontRecord) and (FFonts.Count=0) then FMiscRecords.Add(TSubListRecord.CreateAndAssign(FFonts));
      if (R is TFormatRecord) and (FFormats.Count=0) then FMiscRecords.Add(TSubListRecord.CreateAndAssign(FFormats));

      if (R is TPaletteRecord) then FPaletteCache:=(R as TPaletteRecord);
      if (R is TXFRecord) or (R is TStyleRecord) then FPaletteIndex:=FMiscRecords.Count; //After the last Style record
      if (R is TObProjRecord) then FHasMacro:=true;
      if (R is TCodeNameRecord) then FCodeName:=(R as TCodeNameRecord).SheetName;

      if (R is TBofRecord) then raise Exception.Create(ErrExcelInvalid)
      else if (R is TIgnoreRecord) then FreeAndNil(R)
      else if (R is TBoundSheetRecord) then FBoundSheets.Add(R as TBoundSheetRecord)
      else if (R is TNameRecord) then FNames.Add(R as TNameRecord)
      else if (R is TXFRecord) then FXF.Add(R as TXFRecord)
      else if (R is TFontRecord) then FFonts.Add(R as TFontRecord)
      else if (R is TFormatRecord) then FFormats.Add(R as TFormatRecord)
      else if (R is TEOFRecord) then sEOF:=(R as TEOFRecord)
      else if (R is TSSTRecord) then begin FSST.Load(R as TSSTRecord); FreeAndNil(R);end
      else if (R is TSupBookRecord) then FReferences.AddSupbook(R as TSupBookRecord)
      else if (R is TExternNameRecord) then begin; FReferences.AddExternName(R as TExternNameRecord);end
      else if (R is TExternSheetRecord) then begin; FReferences.AddExternRef(R as TExternSheetRecord); FreeAndNil(R);end
      else if (R is TDrawingGroupRecord) then FDrawingGroup.LoadFromStream(DataStream, RecordHeader, R as TDrawingGroupRecord)
      else if (R is TWindow1Record) then begin; FWindow1:=R as TWindow1Record; FMiscRecords.Add(R); end
      else if (R is T1904Record) then begin; F1904:=R as T1904Record; FMiscRecords.Add(R); end
      else if (R is TBookBoolRecord) then begin; FBookbool:=R as TBookBoolRecord; FMiscRecords.Add(R); end
      else if (R is TPrecisionRecord) then begin; FPrecision:=R as TPrecisionRecord; FMiscRecords.Add(R); end

      else if (R is TTemplateRecord) then begin; FreeAndNil(R); FIsXltTemplate:=true; end

      else FMiscRecords.Add(R);

    except
      FreeAndNil(R);
      Raise;
    end; //Finally

  until RecordId = xlr_EOF;

  sBOF:=First; //Last statement
end;

procedure TWorkbookGlobals.SaveRangeToStream(const DataStream: TOle2File;
  const SheetIndex: integer; const CellRange: TXlsCellRange);
begin
  //Someday this can be optimized to only save texts on the range
  //But even Excel does not do it...
  if (sBOF=nil)or(sEOF=nil) then raise Exception.Create(ErrSectionNotLoaded);

  sBOF.SaveToStream(DataStream);
  if (FIsXltTemplate) then TTemplateRecord.SaveNewRecord(DataStream);

  FMiscRecords.SaveToStream(DataStream);
  //FXF, FFonts and FFormats are saved in FMiscRecords.SaveToStream;

  FBoundSheets.SaveRangeToStream(DataStream, SheetIndex);
  FReferences.SaveToStream(DataStream);
  FNames.SaveToStream(DataStream); //Should be after FBoundSheets.SaveToStream
  //Images are not saved to the clipboard by excel
  //FDrawingGroup.SaveToStream(DataStream);
  FSST.SaveToStream(DataStream);
  sEOF.SaveToStream(DataStream);
end;

procedure TWorkbookGlobals.SaveToStream(const DataStream: TOle2File);
begin
  if (sBOF=nil)or(sEOF=nil) then raise Exception.Create(ErrSectionNotLoaded);

  sBOF.SaveToStream(DataStream);
  if (FIsXltTemplate) then TTemplateRecord.SaveNewRecord(DataStream);
  FMiscRecords.SaveToStream(DataStream);
  //FXF, FFonts and FFormats are saved in FMiscRecords.SaveToStream;

  FBoundSheets.SaveToStream(DataStream);
  FReferences.SaveToStream(DataStream);
  FNames.SaveToStream(DataStream); //Should be after FBoundSheets.SaveToStream
  FDrawingGroup.SaveToStream(DataStream);
  FSST.SaveToStream(DataStream);
  sEOF.SaveToStream(DataStream);
end;

procedure TWorkbookGlobals.SetActiveSheet(const Value: integer);
begin
  if FWindow1<>nil then FWindow1.ActiveSheet:=Value;
end;

procedure TWorkbookGlobals.SetColorPalette(Index: integer;
  const Value: LongWord);
begin
  if FPaletteCache=nil then
  begin
    //We have to create a standard palette first.
    FMiscRecords.Insert(FPaletteIndex, TPaletteRecord.CreateStandard);
    FPaletteCache:=FMiscRecords[FPaletteIndex] as TPaletteRecord;
  end;
  FPaletteCache.Color[Index]:= Value;
end;

procedure TWorkbookGlobals.SetFirstSheetVisible(const index: integer);
begin
  if FWindow1<>nil then FWindow1.FirstSheetVisible:=index;
end;

procedure TWorkbookGlobals.SetIs1904(const Value: boolean);
begin
  if F1904<>nil then F1904.Is1904:=value;
end;

procedure TWorkbookGlobals.SetPrecisionAsDisplayed(const Value: boolean);
begin
  if FPrecision<>nil then FPrecision.PrecisionAsDisplayed:=value;
end;

procedure TWorkbookGlobals.SetSaveExternalLinkValues(const Value: boolean);
begin
  if FBookBool<>nil then FBookBool.SaveExternalLinkValues:=value;
end;

procedure TWorkbookGlobals.SetSheetName(const index: integer;
  const Value: UTF16String);
var
  RealName: UTF16String;
begin
   RealName:=TSheetNameList.MakeValidSheetName(Value);
   FBoundSheets.FSheetNames.Rename(FBoundSheets.BoundSheets.SheetName[index], RealName);
   FBoundSheets.BoundSheets.SheetName[index]:=RealName;
end;

procedure TWorkbookGlobals.SetSheetVisible(const index: integer; const Value: TXlsSheetVisible);
begin
   FBoundSheets.BoundSheets.SheetVisible[index]:=Value;
end;

procedure TWorkbookGlobals.SheetSetOffset(const index: integer; const Offset: LongWord);
begin
  FBoundSheets.BoundSheets[index].SetOffset(Offset);
end;

function TWorkbookGlobals.TotalRangeSize(const SheetIndex: integer; const CellRange: TXlsCellRange): int64;
begin
  Result:= inherited TotalRangeSize(SheetIndex, CellRange) +
      TTemplateRecord.GetSize(FIsXltTemplate) +
      FSST.TotalSize +
      FReferences.TotalSize +
      FBoundSheets.TotalRangeSize(SheetIndex) +
      FMiscRecords.TotalSize +
      FNames.TotalSize+
      //Excel doesnt save images to the clipboard
      //FDrawingGroup.TotalSize+
      //FXF.TotalSize, FFonts.TotalSize and FFormats.TotalSize are not included in FMiscRecords.TotalSize;
      FXF.TotalSize+
      FFonts.TotalSize+
      FFormats.TotalSize;
end;

function TWorkbookGlobals.TotalSize: int64;
begin
  Result:= inherited TotalSize +
      TTemplateRecord.GetSize(FIsXltTemplate) +
      FSST.TotalSize +
      FReferences.TotalSize +
      FBoundSheets.TotalSize +
      FMiscRecords.TotalSize +
      FNames.TotalSize+
      FDrawingGroup.TotalSize+
      //FXF.TotalSize, FFonts.TotalSize and FFormats.TotalSize are not included in FMiscRecords.TotalSize;
      FXF.TotalSize+
      FFonts.TotalSize+
      FFormats.TotalSize;
end;

procedure TWorkbookGlobals.CheckInternalNames(const OptionFlags: Integer);
begin
  //If a name is added and it is internal, we can't trust the ordering and need to delete the country record.
  if (OptionFlags and 32) <> 0 then
    DeleteCountry;
end;

function ContainsAny(const Name: UTF16String; const Chars: WideCharArray): boolean;
var
  i, k: integer;
begin
  for i:=1 to Length(Name) do
  begin
    for k:=0 to Length(Chars)-1 do
    begin
      if Name[i] = Chars[k] then
      begin
        Result:= true;
        exit;
      end;
    end;
  end;
  Result:= false;
end;

class function TWorkbookGlobals.IsValidRangeName(const Name: UTF16String; var OptionFlags: integer): Boolean;
var
  InvalidChars: WideCharArray;
  i: integer;
begin
  if ((Length(Name) < 1)) or (Length(Name) > 254) then
    begin Result := false; exit; end;

  if (Name = 'R') or (Name = 'r') then
    begin Result := false; exit; end;

  if (Length(Name) = 1) and (integer(Name[1 + 0]) <= 13) then //Internal name.
  begin
    OptionFlags:= OptionFlags or 32;
    begin Result := true; exit; end;
  end;

  SetLength (InvalidChars, (65 + 192) - 127);
  FillChar(InvalidChars[0], Length(InvalidChars) * 2, 0);
  for i := 0 to 64 do
    InvalidChars[i] := UTF16Char(i);

  InvalidChars[48] := '{';
  InvalidChars[49] := '/';
  InvalidChars[50] := '}';
  InvalidChars[51] := '[';
  InvalidChars[52] := ']';
  InvalidChars[53] := '~';
  InvalidChars[54] := UTF16Char(160);
  InvalidChars[55] := '{';
  InvalidChars[56] := '{';
  InvalidChars[57] := '{';
  InvalidChars[63] := '{';
  for i := 127 to 191 do
    InvalidChars[(65 + i) - 127] := UTF16Char(i);

  InvalidChars[(65 + 181) - 127] := '{';
  if ContainsAny(Name, InvalidChars) then
    begin Result := false; exit; end;

  if Name[1 + 0] < 'A' then
    begin Result := false; exit; end;


  //Check it is not a valid cell reference.
  if (ord((Name[1])) in [ord('A')..ord('Z'),ord('a')..ord('z')]) then
  begin
    if (Length(Name) < 2) then begin Result := true; exit; end;
    if (ord((Name[2])) in [ord('A')..ord('Z'),ord('a')..ord('z')]) then
    begin
      if (Length(Name) < 3) then begin Result := true; exit; end;
      for i:=3 to Length(Name) do if not(ord(Name[i]) in [ord('0')..ord('9')]) then begin Result := true; exit; end;
    end
    else
    begin
      if (Length(Name) < 2) then begin Result := true; exit; end;
      for i:=2 to Length(Name) do if not(ord(Name[i]) in [ord('0')..ord('9')]) then begin Result := true; exit; end;
    end;
    Result := false; exit;
  end;
  Result:= true;
end;

procedure TWorkbookGlobals.AddName(var Range: TXlsNamedRange; const CellList: pointer);
var
  Options: Integer;
  ValidName: Boolean;
  i: integer;
  rSheet: integer;
begin
  Options := Range.OptionFlags;
  ValidName := IsValidRangeName(Range.Name, Options);
  Range.OptionFlags := Options;
  for i := 0 to FNames.Count - 1 do
  begin
    rSheet := FNames[i].RangeSheet;
    if (rSheet = Range.NameSheetIndex) and (WideUpperCase98(FNames[i].Name) = WideUpperCase98(Range.Name)) then
    begin
      //no need to free the record, the collection will do it.
      FNames[i] := TNameRecord.CreateFromData(Range, Self, CellList);
      exit;
    end;

  end;

  if not ValidName then
    raise Exception.CreateFmt(ErrInvalidNameForARange, [Range.Name]);

  FNames.Add(TNameRecord.CreateFromData(Range, Self, CellList));
  CheckInternalNames(Range.OptionFlags);
end;

function TWorkbookGlobals.GetName(const sheet: Int32; const aName: UTF16String): TNameRecord;
var
  i: Int32;
begin
  for i := FNames.Count - 1 downto 0 do
  begin
    if (FNames[i].RangeSheet = sheet) and (WideUpperCase98(FNames[i].Name) = WideUpperCase98(aName)) then
      begin Result := FNames[i]; exit; end;
  end;

  Result := nil;
end;

procedure TWorkbookGlobals.DeleteCountry;
var
  i: integer;
begin
  for i:= FMiscRecords.Count - 1 downto 0 do
  begin
    if TBaseRecord(FMiscRecords[i]).Id = xlr_Country then
    begin
      FMiscRecords.Delete(i);
      exit;
    end;
  end;
end;

end.
