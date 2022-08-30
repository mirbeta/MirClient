unit tmsUXlsBaseClientData;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, tmsUXlsBaseRecords, tmsUXlsSST, tmsXlsMessages, tmsUOle2Impl;
type
  TBaseClientData = class
  protected
    function GetId: Word;virtual;
    procedure SetId(const Value: Word); virtual;
  public
    RemainingData: TBaseRecord;

    property Id: Word read GetId write SetId;
    procedure ArrangeId(var MaxId: word);virtual;

    procedure Clear; virtual; abstract;
    function CopyTo: TBaseClientData; virtual; abstract;
    procedure LoadFromStream(const DataStream: TOle2File; var RecordHeader: TRecordHeader; const First: TBaseRecord; const SST: TSST); virtual; abstract;
    procedure SaveToStream(const DataStream: TOle2File); virtual; abstract;
    function TotalSize: int64;virtual; abstract;

    procedure ArrangeCopyRowsAndCols(const RowOfs, ColOfs: integer);virtual;
    procedure ArrangeInsertRowsAndCols(const aRowPos, aRowCount, aColPos, aColCount:integer; const SheetInfo: TSheetInfo);virtual; abstract;
    procedure ArrangeCopySheet(const SheetInfo: TSheetInfo);virtual; abstract;

    class function ObjRecord: ClassOfTBaseRecord; virtual;

  end;

  ClassOfTBaseClientData = class of TBaseClientData;


implementation

{ TBaseClientData }

procedure TBaseClientData.ArrangeId(var MaxId: word);
begin

end;

function TBaseClientData.GetId: Word;
begin
  Result:=0;
end;

procedure TBaseClientData.SetId(const Value: Word);
begin

end;

class function TBaseClientData.ObjRecord: ClassOfTBaseRecord;
begin
  Result:= nil;
end;

procedure TBaseClientData.ArrangeCopyRowsAndCols(const RowOfs, ColOfs: integer);
begin

end;

end.

