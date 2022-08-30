unit tmsUXlsNotes;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses SysUtils, tmsUXlsBaseRecords, tmsUXlsRowColEntries, tmsUxlsBaseRecordLists,
     {$IFDEF DELPHIXE3UP} System.Classes, System.Contnrs, System.Types,{$ENDIF}
    tmsUXlsEscher, tmsUEscherRecords, tmsXlsMessages, tmsUFlxMessages;
type
  TNoteRecord = class (TBaseRowColRecord)
  private
    Dwg: TEscherClientDataRecord;
    function GetText: UTF16String;
    procedure SetText(const Value: UTF16String);
  protected
    function DoCopyTo: TBaseRecord; override;
  public
    constructor CreateFromData(const aRow, aCol: integer; const aTxt: UTF16String; const Drawing: TDrawing; Properties: TImageProperties; const sSheet: TObject);
    destructor Destroy;override;
    procedure ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);override;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer; const SheetInfo: TSheetInfo); override;
    procedure FixDwgIds(const Drawing: TDrawing);

    property Text: UTF16String read GetText write SetText;
  end;

  TNoteRecordList = class (TBaseRowColRecordList)
    {$INCLUDE TNoteRecordListHdr.inc}
    procedure FixDwgIds(const Drawing: TDrawing);
  end;

  TNoteList = class (TBaseRowColList) //records are TNoteRecordList
    {$INCLUDE TNoteListHdr.inc}
    constructor Create;
    procedure FixDwgIds(const Drawing: TDrawing);
    procedure AddNewComment(const Row, Col: integer; const Txt: UTF16String; const Drawing: TDrawing; const Properties: TImageProperties; const sSheet: TObject);
  end;



implementation
uses tmsUXlsClientData, tmsUEscherOtherRecords;
{$INCLUDE TNoteRecordListImp.inc}
{$INCLUDE TNoteListImp.inc}

{ TNoteRecord }

procedure TNoteRecord.ArrangeCopyRowsAndCols(const RowOffset, ColOffset: integer);
begin
  if Dwg<>nil then
  begin
    //We only copy DWG if we are copying rows/columns, when we copy sheets we dont have to
    Dwg:=Dwg.CopyDwg(RowOffset, ColOffset, nil) as TEscherClientDataRecord;  //warning
    SetWord(Data, 6, Dwg.ObjId);
  end;
  inherited; //This must be last, so we dont modify row
end;

procedure TNoteRecord.ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount: integer;
  const SheetInfo: TSheetInfo);
begin
  inherited;
  if (Dwg<>nil) and (Dwg.FindRoot<>nil) then Dwg.FindRoot.ArrangeInsertRowsAndCols(aRowPos, aRowCount, aColPos, aColCount, SheetInfo, true, nil); //warning
end;

constructor TNoteRecord.CreateFromData(const aRow, aCol: integer; const aTxt: UTF16String; const Drawing: TDrawing; Properties: TImageProperties; const sSheet: TObject);
var
  aData: PArrayOfByte;
  aDataSize: integer;
begin
  if (aRow<0) or (aRow>Max_Rows) then raise Exception.CreateFmt(ErrXlsIndexOutBounds, [aRow, 'Row', 0, Max_Rows]);
  if (aCol<0) or (aCol>Max_Columns) then raise Exception.CreateFmt(ErrXlsIndexOutBounds, [aCol, 'Column', 0, Max_Columns]);
  aDataSize:=12;

  GetMem(aData, aDataSize);
  try
    SetWord(aData, 0, aRow);
    SetWord(aData, 2, aCol);
    SetWord(aData, 4, 0);   //option flags
    SetWord(aData, 6, 0);   //object id
    SetWord(aData, 8, 0);   //Author
    SetWord(aData, 10, 0);  //rtf for author.

    Create(xlr_NOTE, aData, aDataSize);
  except
    FreeMem(aData);
    raise;
  end; //except

  Dwg:=Drawing.AddNewComment(Properties, sSheet);
  SetWord(Data, 6, Dwg.ObjId);   //object id
  Text:=aTxt;
end;

destructor TNoteRecord.Destroy;
begin
  if Dwg<>nil then
  begin
    if (Dwg.Patriarch=nil) then raise Exception.Create(ErrLoadingEscher);
    Dwg.Patriarch.ContainedRecords.Remove(Dwg.FindRoot);
  end;
  inherited;
end;

function TNoteRecord.DoCopyTo: TBaseRecord;
begin
  Result:=inherited DoCopyTo;
  (Result as TNoteRecord).Dwg:=Dwg;
end;

procedure TNoteRecord.FixDwgIds(const Drawing: TDrawing);
begin
  Dwg:= Drawing.FindObjId(GetWord(Data, 6));
end;

function TNoteRecord.GetText: UTF16String;
var
  R:TEscherRecord;
begin
  if (Dwg=nil) then Result:='' else
  begin
    R:=Dwg.FindRoot;
    if R=nil then Result:='' else
    begin
      R:= Dwg.FindRoot.FindRec(TEscherClientTextBoxRecord);
      if R=nil then Result:='' else Result:= (R as TEscherClientTextBoxRecord).Value;
    end;
  end;
end;

procedure TNoteRecord.SetText(const Value: UTF16String);
var
  R:TEscherRecord;
begin
  if (Dwg=nil) then exit else
  begin
    R:=Dwg.FindRoot;
    if R=nil then exit else
    begin
      R:= R.FindRec(TEscherClientTextBoxRecord);
      if R=nil then exit else (R as TEscherClientTextBoxRecord).Value:=Value;
    end;
  end;
end;

{ TNoteRecordList }

procedure TNoteRecordList.FixDwgIds(const Drawing: TDrawing);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].FixDwgIds(Drawing);
end;

{ TNoteList }
procedure TNoteList.AddNewComment(const Row, Col: integer; const Txt: UTF16String; const Drawing: TDrawing; const Properties: TImageProperties; const sSheet: TObject);
var
  R: TNoteRecord;
begin
  R:=TNoteRecord.CreateFromData(Row, Col, Txt, Drawing, Properties, sSheet);
  try
    AddRecord(R, Row);
  except
    FreeAndNil(R);
    raise;
  end; //Except
end;

constructor TNoteList.Create;
begin
  inherited Create(TNoteRecordList);
end;

procedure TNoteList.FixDwgIds(const Drawing: TDrawing);
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].FixDwgIds(Drawing);
end;

end.
