unit tmsUBreakList;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsXlsMessages, tmsUXlsBaseRecords;

type
  TBreakList=class(TList)
  private
    CurrentPos: integer;
    ZeroPos: integer;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const aZeroPos: integer);

    function CurrentId: integer;
    function CurrentSize: integer;
    function AcumSize: integer;

    procedure IncCurrent;
    procedure Add(const aId, aSize: integer);

    procedure AddToZeroPos(const Delta: integer);
  end;

implementation
type
  TRBreakList=record
    Id, Size, AcumSize: integer;
  end;

  PRBreakList= ^TRBreakList;


{ TBreakList }

function TBreakList.AcumSize: integer;
begin
  if (CurrentPos>= Count)or (CurrentPos<0) then Raise Exception.Create(ErrInternal);
  Result:= PRBreakList(Items[CurrentPos]).AcumSize + ZeroPos + SizeOf(TRecordHeader)*(CurrentPos);
end;

procedure TBreakList.Add(const aId, aSize: integer);
var
  RBreakList: PRBreakList;
begin
  New( RBreakList);
  RBreakList.Id:=aId;
  RBreakList.Size:=aSize;
  RBreakList.AcumSize:=aSize;
  if Count>0 then inc(RBreakList.AcumSize, PRBreakList(Items[Count-1]).AcumSize);
  inherited Add(RBreakList);
end;

procedure TBreakList.AddToZeroPos(const Delta: integer);
begin
  inc(ZeroPos, Delta);
end;

constructor TBreakList.Create(const aZeroPos: integer);
begin
  inherited Create;
  ZeroPos:= aZeroPos;
end;

function TBreakList.CurrentId: integer;
begin
  if (CurrentPos>= Count)or (CurrentPos<0) then Raise Exception.Create(ErrInternal);
  Result:= PRBreakList(Items[CurrentPos]).Id;
end;

function TBreakList.CurrentSize: integer;
begin
  if (CurrentPos+1>= Count)or (CurrentPos+1<0) then Raise Exception.Create(ErrInternal);
  Result:= PRBreakList(Items[CurrentPos+1]).Size;
end;

procedure TBreakList.IncCurrent;
begin
  inc(CurrentPos);
end;

procedure TBreakList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then Dispose(PRBreakList(Ptr));
  inherited Notify(Ptr, Action);
end;

end.
