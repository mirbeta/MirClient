unit tmsUXlsEscher;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsUXlsOtherRecords,
     tmsXlsMessages, tmsUFlxMessages, Classes, SysUtils, tmsUEscherRecords, tmsUXlsSST, tmsUBreakList,
     {$IFDEF DELPHIXE3UP} System.Contnrs, System.Types, {$ENDIF}
     tmsUEscherOtherRecords, tmsUOle2Impl;

type

  TXlsEscherRecord = class (TBaseRecord)
  end;

  TDrawingGroupRecord = class (TXlsEscherRecord)
  end;

  TDrawingRecord = class (TXlsEscherRecord)
  end;


  TDrawingSelectionRecord = class (TXlsEscherRecord)
  end;

  TDrawingGroup= class
  private
    FDggContainer: TEscherContainerRecord;
    FRecordCache: TEscherDwgGroupCache;
    function GetRecordCache: PEscherDwgGroupCache;
  public
    property  RecordCache: PEscherDwgGroupCache read GetRecordCache;

    constructor Create;
    procedure Clear;
    destructor Destroy; override;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TDrawingGroupRecord);
    procedure SaveToStream(const DataStream: TOle2File);
    function TotalSize: int64;

    procedure AddDwg;
    procedure EnsureDwgGroup;
  end;

  TDrawing=class
  private
    FDgContainer: TEscherContainerRecord;
    FRecordCache: TEscherDwgCache;
    FDrawingGroup: TDrawingGroup;
    function GetDrawingName(index: integer): UTF16String;
    function GetDrawingRow(index: integer): integer;
    procedure CreateBasicDrawingInfo;

  public
    procedure Clear;
    constructor Create(const aDrawingGroup: TDrawingGroup);
    destructor Destroy; override;

    procedure CopyFrom(const aDrawing: TDrawing; const dSheet: TObject);
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TDrawingRecord; const SST: TSST);
    procedure SaveToStream(const DataStream: TOle2File);
    function TotalSize: int64;

    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo; const dSheet: TObject);
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);
    procedure InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, RowCount, FirstCol, LastCol, DestCol, ColCount: integer; const SheetInfo: TSheetInfo; const dSheet: TObject);
    procedure DeleteRows(const aRow, aCount: word;const SheetInfo: TSheetInfo; const dSheet: TObject);
    procedure DeleteCols(const aCol, aCount: word;const SheetInfo: TSheetInfo; const dSheet: TObject);

    function FindObjId(const ObjId: word): TEscherClientDataRecord;

    function DrawingCount: integer;
    procedure AssignDrawing(const Index: integer; const Data: ByteArray; const DataType: TXlsImgTypes);
    function GetAnchor(const Index: integer): TClientAnchor;
    procedure SetAnchor(const Index: integer; const aAnchor: TClientAnchor; const sSheet: TObject);
    procedure GetDrawingFromStream(const Index: integer; const Data: TStream; out DataType: TXlsImgTypes);
    property DrawingRow[index: integer]: integer read GetDrawingRow;
    property DrawingName[index: integer]: UTF16String read GetDrawingName;

    procedure DeleteImage(const Index: integer);
    procedure ClearImage(const Index: integer);
    procedure AddImage(Data: ByteArray; DataType: TXlsImgTypes; const Properties: TImageProperties;const Anchor: TFlxAnchorType; const sSheet: TObject);

    procedure RemoveAutoFilter();
    procedure AddAutoFilter(const Row: Int32; const Col1: Int32; const Col2: Int32; const sSheet: TObject);overload;
    procedure AddAutoFilter(const Row: Int32; const Col: Int32; const sSheet: TObject);overload;

    function AddNewComment(const Properties: TImageProperties; const sSheet: TObject): TEscherClientDataRecord;

    procedure SaveObjectCoords(const sSheet: TObject);
    procedure RestoreObjectCoords(const dSheet: TObject);
  end;

implementation
uses tmsUXlsBaseClientData, tmsUXlsClientData;
const
  StEmptyBmp: Array[0..53] of byte = ($28,$0,$0,$0,$1,$0
            ,$0,$0,$1,$0,$0,$0,$1,$0,$1,$0,$0,$0,$0,$0,$0,$0,$0,$0,$12,$B,$0
            ,$0,$12,$B,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$FF,$FF,$FF,$0,$0,$0
            ,$0,$0,$0,$0,$0,$0,$0,$0);

function EmptyBmp: ByteArray;
var
  i : integer;
begin
  SetLength (result, Length (StEmptyBmp));
  for i := 0 to High (StEmptyBmp) do
      result [i] := StEmptyBmp [i];
end;



{ TDrawingGroup }

procedure TDrawingGroup.AddDwg;
begin
  if FRecordCache.Dgg<>nil then inc(FRecordCache.Dgg.FDgg.DwgSaved);
  //PENDING: fix sheets

end;

procedure TDrawingGroup.Clear;
begin
  FreeAndNil(FDggContainer);
end;

constructor TDrawingGroup.Create;
begin
  inherited Create;
end;

destructor TDrawingGroup.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDrawingGroup.EnsureDwgGroup;
const
  DwgCache: TEscherDwgCache= (Destroying: false; MaxObjId:0; Dg: nil; Solver: nil; Patriarch:nil; Anchor: nil; Shape: nil; Obj: nil; Blip: nil);
var
  EscherHeader: TEscherRecordHeader;
  FDgg: TEscherDggRecord;
  BStoreContainer: TEscherBStoreRecord;
  OPTRec:TEscherOPTRecord;
  SplitMenu: TEscherSplitMenuRecord;
begin
  if FDggContainer=nil then  // there is already a DwgGroup
  begin
    //DggContainer
    EscherHeader.Pre:=$F;
    EscherHeader.Id:=MsofbtDggContainer;
    EscherHeader.Size:=0;
    FDggContainer:=TEscherContainerRecord.Create(EscherHeader, RecordCache, @DwgCache ,nil);
    FDggContainer.LoadedDataSize:=EscherHeader.Size;
  end;

  if FDggContainer.FindRec(TEscherDggRecord)=nil then
  begin
    //Dgg
    FDgg:=TEscherDggRecord.CreateFromData(RecordCache, @DwgCache ,FDggContainer);
    FDggContainer.ContainedRecords.Add(FDgg);
  end;

  if FDggContainer.FindRec(TEscherBStoreRecord)=nil then
  begin
    // BStoreContainer
    EscherHeader.Pre:=$2F;
    EscherHeader.Id:=MsofbtBstoreContainer;
    EscherHeader.Size:=0;
    BStoreContainer:=TEscherBStoreRecord.Create(EscherHeader, RecordCache, @DwgCache ,FDggContainer);
    BStoreContainer.LoadedDataSize:=EscherHeader.Size;
    FDggContainer.ContainedRecords.Add(BStoreContainer);
  end;

  if FDggContainer.FindRec(TEscherOPTRecord)=nil then
  begin
    //OPT
    OPTRec:=TEscherOPTRecord.GroupCreateFromData(RecordCache, @DwgCache, FDggContainer);
    FDggContainer.ContainedRecords.Add(OPTRec);
  end;

  if FDggContainer.FindRec(TEscherSplitMenuRecord)=nil then
  begin
    //SplitMenuColors
    SplitMenu:=TEscherSplitMenuRecord.CreateFromData(RecordCache, @DwgCache, FDggContainer);
    FDggContainer.ContainedRecords.Add(SplitMenu);
  end;

end;

function TDrawingGroup.GetRecordCache: PEscherDwgGroupCache;
begin
  Result:=@FRecordCache;
end;

procedure TDrawingGroup.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TDrawingGroupRecord);
const
  DwgCache: TEscherDwgCache= (Destroying: false; MaxObjId:0; Dg: nil; Solver: nil; Patriarch:nil; Anchor: nil; Shape: nil; Obj: nil; Blip: nil);
var
  aPos: integer;
  EscherHeader: TEscherRecordHeader;
  MyRecord, CurrentRecord: TBaseRecord;
begin
  if FDggContainer<>nil then raise Exception.Create(ErrExcelInvalid);
  aPos:=0;
  MyRecord:= First; CurrentRecord:= First;
  try
    ReadMem(MyRecord, aPos, SizeOf(EscherHeader), @EscherHeader);
    FDggContainer:= TEscherContainerRecord.Create(EscherHeader, RecordCache, @DwgCache ,nil);
    while not FDggContainer.Loaded do
    begin
      if (MyRecord.Continue=nil) and (aPos=MyRecord.DataSize) then
      begin
        if CurrentRecord<> First then FreeAndNil(CurrentRecord);
        CurrentRecord:=LoadRecords(DataStream, RecordHeader);
        MyRecord:= CurrentRecord;
        aPos:=0;
        if not(MyRecord is TDrawingGroupRecord) then raise Exception.Create(ErrExcelInvalid);
      end;

      FDggContainer.Load(MyRecord, aPos);

    end; //while
  finally
    if CurrentRecord<>First then FreeAndNil(CurrentRecord);
  end; //finally

  First.Free;   //last statment
end;

procedure TDrawingGroup.SaveToStream(const DataStream: TOle2File);
var
  BreakList: TBreakList;
  NextPos, RealSize, NewDwg: integer;
begin
  if FDggContainer=nil then exit;
  BreakList:= TBreakList.Create(DataStream.Position);
  try
    NextPos:=0;
    RealSize:=0;
    NewDwg:= xlr_MSODRAWINGGROUP;
    FDggContainer.SplitRecords(NextPos, RealSize, NewDwg, BreakList);
    BreakList.Add(0, NextPos);
    FDggContainer.SaveToStream(DataStream, BreakList);
  finally
    FreeAndNil(BreakList);
  end; //finally
end;

function TDrawingGroup.TotalSize: int64;
var
  NextPos, RealSize, NewDwg: integer;
begin
  if FDggContainer=nil then begin Result:=0; exit;end;

  NextPos:=0; RealSize:=0; NewDwg:= xlr_MSODRAWINGGROUP;
  FDggContainer.SplitRecords(NextPos, RealSize, NewDwg, nil);
  Result:=RealSize;
end;

{ TDrawing }

procedure TDrawing.ArrangeCopySheet(const SheetInfo: TSheetInfo);
begin
  if (FRecordCache.Obj<> nil) then
    FRecordCache.Obj.ArrangeCopySheet(SheetInfo);
end;

procedure TDrawing.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo; const dSheet: TObject);
begin
  if (FRecordCache.Anchor<> nil) and (SheetInfo.FormulaSheet= SheetInfo.InsSheet)then
    FRecordCache.Anchor.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo, false, dSheet);
  if (FRecordCache.Obj<> nil) then
    FRecordCache.Obj.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo, false, dSheet);
end;

procedure TDrawing.AssignDrawing(const Index: integer; const Data: ByteArray;
  const DataType: TXlsImgTypes);
begin
  if Length(Data)= 0 then ClearImage(Index)  //XP crashes with a 0 byte image.
  else FRecordCache.Blip[Index].ReplaceImg(Data, DataType);
end;

procedure TDrawing.DeleteImage(const Index: integer);
begin
  if FRecordcache.Blip=nil then exit;
  if (FRecordCache.Patriarch=nil) then raise Exception.Create(ErrLoadingEscher);
  FRecordCache.Patriarch.ContainedRecords.Remove(FRecordCache.Blip[Index].FindRoot);
end;

procedure TDrawing.ClearImage(const Index: integer);
begin
  FRecordCache.Blip[Index].ReplaceImg(ByteArray(EmptyBmp), xli_Bmp);
end;

procedure TDrawing.Clear;
begin
  FreeAndNil(FDgContainer);
  //Order is important... Cache should be freed after DgContainer
  FreeAndNil(FRecordCache.Anchor);
  FreeAndNil(FRecordCache.Obj);
  FreeAndNil(FRecordCache.Shape);
  FreeAndNil(FRecordCache.Blip);
end;

procedure TDrawing.CopyFrom(const aDrawing: TDrawing; const dSheet: TObject);
begin
  Clear;
  FRecordCache.MaxObjId:=0;
  FRecordCache.Dg:=nil; FRecordCache.Patriarch:=nil;

  if aDrawing.FRecordCache.Anchor<>nil then
  begin
    FRecordCache.Anchor:= TEscherAnchorCache.Create;
    FRecordCache.Obj:= TEscherObjCache.Create;
    FRecordCache.Shape:= TEscherShapeCache.Create;
    FRecordCache.Blip:=TEscherOPTCache.Create;
  end;

  if aDrawing.FDgContainer=nil then FreeAndNil(FDgcontainer) else
  begin
    aDrawing.FDgContainer.ClearCopiedTo;
    FDgContainer:=aDrawing.FDgContainer.CopyTo(@FRecordCache, 0,0, dSheet) as TEscherContainerRecord;
    FRecordCache.Shape.Sort; // only here the values are loaded...
    if FRecordCache.Solver<>nil then FRecordCache.Solver.CheckMax(aDrawing.FRecordCache.Solver.MaxRuleId);

    FDrawingGroup.AddDwg;
  end;
  //MADE: change cache
end;

constructor TDrawing.Create(const aDrawingGroup: TDrawingGroup);
begin
  inherited Create;
  FDrawingGroup:=aDrawingGroup;
  FRecordCache.Destroying:=false;
end;

procedure TDrawing.DeleteRows(const aRow, aCount: word;
  const SheetInfo: TSheetInfo; const dSheet: TObject);
var i: integer;
begin
  if FRecordCache.Anchor=nil then exit;
  for i:= FRecordCache.Anchor.Count-1 downto 0 do
    if FRecordCache.Anchor[i].AllowDelete(aRow, aRow+aCount-1,0,Max_Columns+1)then
    begin
      if (FRecordCache.Patriarch=nil) then raise Exception.Create(ErrLoadingEscher);
      FRecordCache.Patriarch.ContainedRecords.Remove(FRecordCache.Anchor[i].FindRoot);
    end;

  ArrangeInsertRowsAndCols(aRow, -aCount, 0,0, SheetInfo, dSheet);
end;

procedure TDrawing.DeleteCols(const aCol, aCount: word;
  const SheetInfo: TSheetInfo; const dSheet: TObject);
var i: integer;
begin
  //MADE: delete cols
  //MADE: Arreglar los continues...
  //MADE: Conectores
  if FRecordCache.Anchor=nil then exit;
  for i:= FRecordCache.Anchor.Count-1 downto 0 do
    if FRecordCache.Anchor[i].AllowDelete(0, Max_Rows+1, aCol, aCol+aCount-1)then
    begin
      if (FRecordCache.Patriarch=nil) then raise Exception.Create(ErrLoadingEscher);
      FRecordCache.Patriarch.ContainedRecords.Remove(FRecordCache.Anchor[i].FindRoot);
    end;

  ArrangeInsertRowsAndCols(0,0,aCol, -aCount, SheetInfo, dSheet);
end;

destructor TDrawing.Destroy;
begin
  FRecordCache.Destroying:=true;
  Clear;
  inherited;
end;

function TDrawing.DrawingCount: integer;
begin
  if FRecordCache.Blip<>nil then Result:=FRecordCache.Blip.Count else Result:=0;
end;

function TDrawing.FindObjId(const ObjId: word): TEscherClientDataRecord;
var
  i: integer;
begin
  for i:=0 to FRecordCache.Obj.Count-1 do if FRecordCache.Obj[i].ObjId=ObjId then
  begin
    Result:=FRecordCache.Obj[i];
    exit;
  end;
  Result:=nil;
end;

function TDrawing.GetAnchor(const Index: integer): TClientAnchor;
begin
  Assert(Index<FRecordCache.Blip.Count,'Index out of range');
  Result:=FRecordCache.Blip[index].GetAnchor;
end;

procedure TDrawing.SetAnchor(const Index: integer; const aAnchor: TClientAnchor; const sSheet: TObject);
begin
  Assert(Index<FRecordCache.Blip.Count,'Index out of range');
  FRecordCache.Blip[index].SetAnchor(aAnchor, sSheet);
end;

procedure TDrawing.GetDrawingFromStream(const Index: integer; const Data: TStream; out DataType: TXlsImgTypes);
begin
  Assert(Index<FRecordCache.Blip.Count,'Index out of range');
  FRecordCache.Blip[index].GetImageFromStream(Data, DataType);
end;

function TDrawing.GetDrawingName(index: integer): UTF16String;
begin
  Assert(Index<FRecordCache.Blip.Count,'Index out of range');
  Result:=FRecordCache.Blip[index].ShapeName;
end;

function TDrawing.GetDrawingRow(index: integer): integer;
begin
  Assert(Index<FRecordCache.Blip.Count,'Index out of range');
  Result:=FRecordCache.Blip[index].Row;
end;

procedure TDrawing.InsertAndCopyRowsAndCols(const FirstRow, LastRow, DestRow, RowCount, FirstCol, LastCol, DestCol,
  ColCount: integer; const SheetInfo: TSheetInfo; const dSheet: TObject);
var
  i,k, myDestRow, myFirstRow, myLastRow, myDestCol, myFirstCol, myLastCol: integer;
begin
  if (FDgContainer=nil) or (FRecordCache.Anchor= nil) then exit;  //no drawings on this sheet

  if DestRow>FirstRow then
  begin
    myFirstRow:=FirstRow; myLastRow:=LastRow;
  end else
  begin
    myFirstRow:=FirstRow+RowCount*(LastRow-FirstRow+1);
    myLastRow:=LastRow+RowCount*(LastRow-FirstRow+1);
  end;

  if DestCol>FirstCol then
  begin
    myFirstCol:=FirstCol; myLastCol:=LastCol;
  end else
  begin
    myFirstCol:=FirstCol+ColCount*(LastCol-FirstCol+1);
    myLastCol:=LastCol+ColCount*(LastCol-FirstCol+1);
  end;

  //Insert cells
  ArrangeInsertRowsAndCols(DestRow, RowCount*(LastRow-FirstRow+1), DestCol, ColCount*(LastCol-FirstCol+1), SheetInfo, dSheet);

  //Copy the images
  //First the rows...
  myDestRow:=DestRow;
  for k:= 0 to RowCount-1 do
  begin
    FDgContainer.ClearCopiedTo;
    for i:= 0 to FRecordCache.Anchor.Count-1 do
      if FRecordCache.Anchor[i].AllowCopy(myFirstRow, myLastRow, 0, Max_Columns+1)then
      begin
         FRecordCache.Anchor[i].CopyDwg(myDestRow-myFirstRow,0, dSheet);
      end;
    inc(myDestRow, (LastRow-FirstRow+1));
    if FRecordCache.Solver<>nil then FRecordCache.Solver.ArrangeCopyRowsAndCols(dSheet);
  end;

  //Now the columns... as we already copied the rows, now we will make an array of images
  myDestCol:=DestCol;
  for k:= 0 to ColCount-1 do
  begin
    FDgContainer.ClearCopiedTo;
    for i:= 0 to FRecordCache.Anchor.Count-1 do
      if FRecordCache.Anchor[i].AllowCopy(0, Max_Rows+1, myFirstCol, myLastCol)then
      begin
         FRecordCache.Anchor[i].CopyDwg(0, myDestCol-myFirstCol, dSheet);
      end;
    inc(myDestCol, (LastCol-FirstCol+1));
    if FRecordCache.Solver<>nil then FRecordCache.Solver.ArrangeCopyRowsAndCols(dSheet);
  end;

end;

procedure TDrawing.CreateBasicDrawingInfo;
var
  EscherHeader: TEscherRecordHeader;
  Dg: TEscherDgRecord;
  SPRec: TEscherSpContainerRecord;
  SPgrRec:TEscherDataRecord;
  SP: TEscherSPRecord;
  DgId: integer;
  FirstId: Int64;
begin
  Assert (FDrawingGroup<>nil,'DrawingGroup can''t be nil');
  FRecordCache.MaxObjId:=0;
  FRecordCache.Dg:=nil; FRecordCache.Patriarch:=nil; FRecordCache.Solver:=nil;

  FRecordCache.Anchor:= TEscherAnchorCache.Create;
  FRecordCache.Obj:= TEscherObjCache.Create;
  FRecordCache.Shape:= TEscherShapeCache.Create;
  FRecordCache.Blip:=TEscherOPTCache.Create;

  EscherHeader.Pre:=$F;
  EscherHeader.Id:=MsofbtDgContainer;
  EscherHeader.Size:=0;
  FDgContainer:=TEscherContainerRecord.Create(EscherHeader, FDrawingGroup.RecordCache, @FRecordCache ,nil);
  FDrawingGroup.AddDwg;

  //Add required records...
  FDrawingGroup.RecordCache.Dgg.GetNewDgIdAndCluster(DgId, FirstId);
  Dg:=TEscherDgRecord.CreateFromData(0, DgId, FirstId, FDrawingGroup.RecordCache, @FRecordCache, FDgContainer);
  FDgContainer.ContainedRecords.Add(Dg);

  EscherHeader.Pre:=$F;
  EscherHeader.Id:=MsofbtSpgrContainer;
  EscherHeader.Size:=0;
  FRecordCache.Patriarch:= TEscherSpgrContainerRecord.Create(EscherHeader, FDrawingGroup.RecordCache, @FRecordCache, FDgContainer);
  FDgContainer.ContainedRecords.Add(FRecordCache.Patriarch);

  EscherHeader.Id:=MsofbtSpContainer;
  EscherHeader.Pre:=$F;
  EscherHeader.Size:=0; //Size for a container is calculated later
  SPRec:=TEscherSpContainerRecord.Create(EscherHeader, FDrawingGroup.RecordCache, @FRecordCache, FRecordCache.Patriarch);
  SPRec.LoadedDataSize:=EscherHeader.Size;
  FRecordCache.Patriarch.ContainedRecords.Add(SPRec);

  EscherHeader.Id:=MsofbtSpgr;
  EscherHeader.Pre:=$1;
  EscherHeader.Size:=16;
  SPgrRec:=TEscherDataRecord.Create(EscherHeader, FDrawingGroup.RecordCache, @FRecordCache, FRecordCache.Patriarch);
  SPgrRec.LoadedDataSize:=EscherHeader.Size;
  SPgrRec.ClearData;
  SPRec.ContainedRecords.Add(SPgrRec);

  SP:=TEscherSPRecord.CreateFromData($2,FRecordCache.Dg.IncMaxShapeId, $5 , FDrawingGroup.RecordCache, @FRecordCache, SPRec);
  SPRec.ContainedRecords.Add(SP);


end;

procedure TDrawing.AddImage(Data: ByteArray; DataType: TXlsImgTypes; const Properties: TImageProperties;const Anchor: TFlxAnchorType; const sSheet: TObject);
var
  SPRec: TEscherSpContainerRecord;
  AnchorRec: TEscherClientAnchorRecord;
  RecordHeader: TEscherRecordHeader;
  ClientAnchor: TClientAnchor;
  ClientData: TEscherClientDataRecord;
  SP: TEscherSPRecord;
  OPTRec:TEscherOPTRecord;
begin
  if Length(Data)=0 then
  begin
    Data:=EmptyBmp;
    DataType:=xli_Bmp;
  end;
  if (FDgContainer=nil) or (FRecordCache.Anchor= nil) then //no drawings on this sheet
    CreateBasicDrawingInfo;

  if (FRecordCache.Patriarch=nil) then raise Exception.Create(ErrLoadingEscher);

  RecordHeader.Id:=MsofbtSpContainer;
  RecordHeader.Pre:=$F;
  RecordHeader.Size:=0; //Size for a container is calculated later
  SPRec:=TEscherSpContainerRecord.Create(RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, FRecordCache.Patriarch);
  SPRec.LoadedDataSize:=RecordHeader.Size;

  SP:=TEscherSPRecord.CreateFromData($04B2, FRecordCache.Dg.IncMaxShapeId, $A00 , FDrawingGroup.RecordCache, @FRecordCache, SPRec);
  SPRec.ContainedRecords.Add(SP);

  OPTRec:=TEscherOPTRecord.CreateFromDataImg(Data, DataType, Properties.FileName, FDrawingGroup.RecordCache, @FRecordCache, SPRec);
  SPRec.ContainedRecords.Add(OPTRec);

  RecordHeader.Id:=MsofbtClientAnchor;
  RecordHeader.Pre:=0;
  RecordHeader.Size:=SizeOf(TClientAnchor);
  case Anchor of
    at_MoveAndResize: ClientAnchor.Flag:=00;
    at_DontMoveAndDontResize: ClientAnchor.Flag:=03;
    else ClientAnchor.Flag:=02;
  end; //case

  ClientAnchor.Col1:=Properties.Col1;
  ClientAnchor.Dx1:=Properties.dx1;
  ClientAnchor.Col2:=Properties.Col2;
  ClientAnchor.Dx2:=Properties.dx2;
  ClientAnchor.Row1:=Properties.Row1;
  ClientAnchor.Dy1:=Properties.dy1;
  ClientAnchor.Row2:=Properties.Row2;
  ClientAnchor.Dy2:=Properties.dy2;
  AnchorRec:=TEscherClientAnchorRecord.CreateFromData(ClientAnchor, RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, SPRec, sSheet);
  SPRec.ContainedRecords.Add(AnchorRec);


  RecordHeader.Id:=MsofbtClientData;
  RecordHeader.Pre:=0;
  RecordHeader.Size:=0;
  ClientData:= TEscherClientDataRecord.Create(RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, SPRec);
  ClientData.AssignClientData(TMsObj.CreateEmptyImg(FRecordCache.MaxObjId));
  ClientData.LoadedDataSize:=RecordHeader.Size;
  SPRec.ContainedRecords.Add(ClientData);
  FRecordCache.Patriarch.ContainedRecords.Add(SPRec);
end;

procedure TDrawing.RemoveAutoFilter();
var
  i: Int32;
  obj0: TEscherClientDataRecord;
  ClientData: TMsObj;
begin
  if FRecordCache.Obj = nil then
    exit;

  if (FRecordCache.Patriarch=nil) then raise Exception.Create(ErrLoadingEscher);

  for i := FRecordCache.Obj.Count - 1 downto 0 do
  begin
    obj0 := FRecordCache.Obj[i];
    if obj0.ClientData = nil then continue;

    if not (obj0.ClientData is TMsObj) then continue;

    ClientData := TMsObj(obj0.ClientData);

    if ClientData.IsAutoFilter then
    begin
      FRecordCache.Patriarch.ContainedRecords.Remove(obj0.FindRoot);
    end;
  end;
end;

procedure TDrawing.AddAutoFilter(const Row: Int32; const Col1: Int32; const Col2: Int32; const sSheet: TObject);
var
  i: Int32;
begin
  for i := Col1 to Col2 do
  begin
    AddAutoFilter(Row, i, sSheet);
  end;
end;

procedure TDrawing.AddAutoFilter(const Row: Int32; const Col: Int32; const sSheet: TObject);
var
  RecordHeader: TEscherRecordHeader;
  SPRec: TEscherSpContainerRecord;
  SP: TEscherSpRecord;
  OPTRec: TEscherOPTRecord;
  ClientAnchor: TClientAnchor;
  AnchorRec: TEscherClientAnchorRecord;
  Obj: TEscherClientDataRecord;
  aMsObj: TMsObj;
begin
  FDrawingGroup.EnsureDwgGroup;
  if (FDgContainer = nil) or (FRecordCache.Anchor = nil) then  //no drawings on this sheet
    CreateBasicDrawingInfo;

  RecordHeader.Id := Int32(MsofbtSpContainer);
  RecordHeader.Pre := 15;
  RecordHeader.Size := 0;  //Size for a container is calculated later

  SPRec:=TEscherSpContainerRecord.Create(RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, FRecordCache.Patriarch);
  try
    SPRec.LoadedDataSize:=RecordHeader.Size;

    SP:=TEscherSPRecord.CreateFromData($0C92, FRecordCache.Dg.IncMaxShapeId, $A00 , FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      SPRec.ContainedRecords.Add(SP);
    except
      FreeAndNil(SP);
      raise;
    end; //except

    OPTRec:=TEscherOPTRecord.CreateFromDataAutoFilter(FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      SPRec.ContainedRecords.Add(OPTRec);
    except
      FreeAndNil(OPTRec);
      raise;
    end; //except

    RecordHeader.Id:=MsofbtClientAnchor;
    RecordHeader.Pre:=0;
    RecordHeader.Size:=SizeOf(TClientAnchor);
    ClientAnchor.Flag:=01; //AutoFilters have a "1" as flag. This is not documented.

    ClientAnchor.Col1:=Col;
    ClientAnchor.Dx1:=0;
    ClientAnchor.Col2:=Col + 1;
    ClientAnchor.Dx2:=0;
    ClientAnchor.Row1:=Row;
    ClientAnchor.Dy1:=0;
    ClientAnchor.Row2:=Row + 1;
    ClientAnchor.Dy2:=0;
    AnchorRec:=TEscherClientAnchorRecord.CreateFromData(ClientAnchor, RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, SPRec, sSheet);
    try
      SPRec.ContainedRecords.Add(AnchorRec);
    except
      FreeAndNil(AnchorRec);
      raise;
    end;

    Obj:=TEscherClientDataRecord.CreateFromData(FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      aMsObj:=TMsObj.CreateEmptyAutoFilter(FRecordCache.MaxObjId);
      try
        Obj.AssignClientData(aMsObj);
      except
        FreeAndNil(aMsObj);
        raise;
      end; //Except

      SPRec.ContainedRecords.Add(Obj);
    except
      FreeAndNil(Obj);
      raise;
    end;

    FRecordCache.Patriarch.ContainedRecords.Add(SPRec);
  except
    FreeAndNil(SPRec);
    raise;
  end; //except
end;


procedure TDrawing.LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader;
  const First: TDrawingRecord; const SST: TSST);
var
  aPos, CdPos: integer;
  EscherHeader: TEscherRecordHeader;
  MyRecord, CurrentRecord, R, CdRecord: TBaseRecord;
  FClientData: TBaseClientData;
  ClientType: ClassOfTBaseClientData;
begin
  Assert (FDrawingGroup<>nil,'DrawingGroup can''t be nil');
  if FDgContainer<>nil then raise Exception.Create(ErrExcelInvalid);

  FRecordCache.MaxObjId:=0;
  FRecordCache.Dg:=nil; FRecordCache.Patriarch:=nil; FRecordCache.Solver:=nil;
  FRecordCache.Anchor:= TEscherAnchorCache.Create;
  FRecordCache.Obj:= TEscherObjCache.Create;
  FRecordCache.Shape:= TEscherShapeCache.Create;
  FRecordCache.Blip:= TEscherOPTCache.Create;

  aPos:=0;
  MyRecord:= First; CurrentRecord:= First;
  try
    ReadMem(MyRecord, aPos, SizeOf(EscherHeader), @EscherHeader);
    FDgContainer:= TEscherContainerRecord.Create(EscherHeader, FDrawingGroup.RecordCache, @FRecordCache ,nil);
    while (not FDgContainer.Loaded) or FDgContainer.WaitingClientData(ClientType) do
    begin
      if not FDgContainer.WaitingClientData(ClientType) then
      begin
        if (MyRecord.Continue=nil) and (aPos=MyRecord.DataSize) then
        begin
          if CurrentRecord<> First then FreeAndNil(CurrentRecord);
          CurrentRecord:=LoadRecords(DataStream, RecordHeader);
          MyRecord:= CurrentRecord;
          aPos:=0;
          if not(MyRecord is TDrawingRecord) then
            raise Exception.Create(ErrExcelInvalid);
        end;
        FDgContainer.Load(MyRecord, aPos);
      end else
      begin
        if not ((MyRecord.Continue=nil) and (aPos=MyRecord.DataSize)) then raise Exception.Create(ErrExcelInvalid);

         R:=LoadRecords(DataStream, RecordHeader);
         try
           if (R is ClientType.ObjRecord) then
           begin
             FClientData:= ClientType.Create;
             try
               FClientData.LoadFromStream(DataStream, RecordHeader, R , SST);
               FDgContainer.AssignClientData(FClientData);
               if FClientData.RemainingData<>nil then
               begin
                 CdRecord:=FClientData.RemainingData; //we dont have to free this
                 CdPos:=0;
                 FDgContainer.Load(CdRecord, CdPos);
               end;
             except
               FreeAndNil(FClientData);
               raise;
             end; //except
           end else raise Exception.Create(ErrInvalidDrawing);
         except
           FreeAndNil(R);
           raise;
         end; //Except
      end;

    end; //while
  finally
    if CurrentRecord<>First then FreeAndNil(CurrentRecord);
  end; //finally

  FRecordCache.Shape.Sort; // only here the values are loaded...
  if FRecordCache.Solver <>nil then FRecordCache.Solver.FixPointers;


  //PENDING: Wmf, emf

  First.Free;   //last statment
end;

procedure TDrawing.SaveToStream(const DataStream: TOle2File);
var
  BreakList: TBreakList;
  NextPos, RealSize, NewDwg: integer;
begin
  if FDgContainer=nil then exit;
  BreakList:= TBreakList.Create(DataStream.Position);
  try
    NextPos:=0;
    RealSize:=0;
    NewDwg:= xlr_MSODRAWING;
    FDgContainer.SplitRecords(NextPos, RealSize, NewDwg, BreakList);
    BreakList.Add(0, NextPos);
    FDgContainer.SaveToStream(DataStream, BreakList);
  finally
    FreeAndNil(BreakList);
  end; //finally
end;

function TDrawing.TotalSize: int64;
var
  NextPos, RealSize, NewDwg: integer;
begin
  if FDgContainer=nil then begin Result:=0; exit;end;

  NextPos:=0; RealSize:=0; NewDwg:= xlr_MSODRAWINGGROUP;
  FDgContainer.SplitRecords(NextPos, RealSize, NewDwg, nil);
  Result:=RealSize;
end;

function TDrawing.AddNewComment(const Properties: TImageProperties; const sSheet: TObject): TEscherClientDataRecord;
var
  aTXO: TTXO;
  aMsObj: TMsObj;
  SP: TEscherSPRecord;
  SPRec: TEscherSpContainerRecord;
  RecordHeader: TEscherRecordHeader;
  TXORec: TEscherClientTextBoxRecord;
  Obj: TEscherClientDataRecord;
  ClientAnchor: TClientAnchor;
  AnchorRec: TEscherClientAnchorRecord;
  OPTRec:TEscherOPTRecord;
begin
  FDrawingGroup.EnsureDwgGroup;
  if (FDgContainer=nil) or (FRecordCache.Anchor= nil) then //no drawings on this sheet
    CreateBasicDrawingInfo;

  RecordHeader.Id:=MsofbtSpContainer;
  RecordHeader.Pre:=$F;
  RecordHeader.Size:=0; //Size for a container is calculated later
  SPRec:=TEscherSpContainerRecord.Create(RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, FRecordCache.Patriarch);
  try
    SPRec.LoadedDataSize:=RecordHeader.Size;

    SP:=TEscherSPRecord.CreateFromData($0CA2, FRecordCache.Dg.IncMaxShapeId, $A00 , FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      SPRec.ContainedRecords.Add(SP);
    except
      FreeAndNil(SP);
      raise;
    end; //except

    OPTRec:=TEscherOPTRecord.CreateFromDataNote(FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      SPRec.ContainedRecords.Add(OPTRec);
    except
      FreeAndNil(OPTRec);
      raise;
    end; //except

    RecordHeader.Id:=MsofbtClientAnchor;
    RecordHeader.Pre:=0;
    RecordHeader.Size:=SizeOf(TClientAnchor);
    ClientAnchor.Flag:=03;

    ClientAnchor.Col1:=Properties.Col1;
    ClientAnchor.Dx1:=Properties.dx1;
    ClientAnchor.Col2:=Properties.Col2;
    ClientAnchor.Dx2:=Properties.dx2;
    ClientAnchor.Row1:=Properties.Row1;
    ClientAnchor.Dy1:=Properties.dy1;
    ClientAnchor.Row2:=Properties.Row2;
    ClientAnchor.Dy2:=Properties.dy2;
    AnchorRec:=TEscherClientAnchorRecord.CreateFromData(ClientAnchor, RecordHeader, FDrawingGroup.RecordCache, @FRecordCache, SPRec, sSheet);
    try
      SPRec.ContainedRecords.Add(AnchorRec);
    except
      FreeAndNil(AnchorRec);
      raise;
    end;

    Obj:=TEscherClientDataRecord.CreateFromData(FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      aMsObj:=TMsObj.CreateEmptyNote(FRecordCache.MaxObjId);
      try
        Obj.AssignClientData(aMsObj);
      except
        FreeAndNil(aMsObj);
        raise;
      end; //Except

      SPRec.ContainedRecords.Add(Obj);
    except
      FreeAndNil(Obj);
      raise;
    end;

    TXORec:= TEscherClientTextBoxRecord.CreateFromData(FDrawingGroup.RecordCache, @FRecordCache, SPRec);
    try
      aTXO:=TTXO.CreateFromData;
      try
        TXORec.AssignClientData(aTXO);
      except
        FreeAndNil(aTXO);
        raise;
      end;
      SPRec.ContainedRecords.Add(TXORec);
    except
      FreeAndNil(TXORec);
      raise;
    end; //except

    FRecordCache.Patriarch.ContainedRecords.Add(SPRec);
  except
    FreeAndNil(SPRec);
    raise;
  end; //except

  Result:=Obj;
end;


procedure TDrawing.RestoreObjectCoords(const dSheet: TObject);
var
  i: integer;
begin
	if (FRecordCache.Patriarch=nil) then exit;
  for i := FRecordCache.Anchor.Count - 1 downto 0 do
	begin
		FRecordCache.Anchor[i].RestoreObjectCoords(dSheet);
  end;
end;

procedure TDrawing.SaveObjectCoords(const sSheet: TObject);
var
  i: integer;
begin
	if (FRecordCache.Patriarch=nil) then exit;
  for i := FRecordCache.Anchor.Count - 1 downto 0 do
	begin
		FRecordCache.Anchor[i].SaveObjectCoords(sSheet);
  end;
end;

end.
