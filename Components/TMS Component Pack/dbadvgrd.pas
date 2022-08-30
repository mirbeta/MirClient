{**************************************************************************}
{ TDBADVSTRINGGRID component                                               }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by                                                               }
{    TMS Software                                                          }
{    copyright © 1999-2012                                                 }
{    Email : info@tmssoftware.com                                          }
{    Web : http://www.tmssoftware.com                                      }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}
                      
unit DBAdvGrd;

interface

{$I TMSDEFS.INC}

// {$DEFINE TMSDEBUG}

uses
  Windows, WinTypes, SysUtils, Messages, Classes, Controls, Grids, DB,
  Graphics, BaseGrid, AdvGrid, AdvObj, Dialogs, DBConsts, DBCtrls
  {$IFDEF TMSDEBUG}
  , TMSUtil
  {$ENDIF}
  ;

const
  MAX_FIELDS = 255;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 7; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 14; // Build nr.
  DATE_VER = 'October, 2004'; // Build month

  // version history
  // 1.7.0.14 : Deprecated

type
  EDBAdvGridError = class(Exception);

  TDBAdvStringGrid = class;

  { TStringGridField }
  TStringGridField = class(TCollectionItem)
  private
    FFieldName:string;
    FTitle:string;
    FColor: TColor;
    FFont: TFont;
    FReadOnly: Boolean;
    FAlignment: TAlignment;
    FShowBands: Boolean;
    FEditor: TEditorType;
    FEditMask: string;
    FTag: Integer;
    FComboItems: TStringList;
    FSpinMax: Integer;
    FSpinStep: Integer;
    FSpinMin: Integer;
    FEditLink: TEditLink;
    procedure SetFieldName(const value:string);
    procedure SetTitle(const value:string);
    procedure Changed;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetShowBands(const Value: Boolean);
    procedure SetComboItems(const Value: TStringList);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Color: TColor read FColor write SetColor;
    property ComboItems: TStringList read FComboItems write SetComboItems;
    property EditLink: TEditLink read FEditLink write FEditLink;
    property EditMask: string read FEditMask write FEditMask;
    property Editor: TEditorType read FEditor write FEditor;
    property FieldName:string read fFieldName write SetFieldName;
    property Font: TFont read FFont write SetFont;
    property Title: string read fTitle write SetTitle;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property ShowBands: Boolean read FShowBands write SetShowBands;
    property SpinMin: Integer read FSpinMin write FSpinMin;
    property SpinMax: Integer read FSpinMax write FSpinMax;
    property SpinStep: Integer read FSpinStep write FSpinStep;
    property Tag: Integer read FTag write FTag;
  end;

  { TStringGridFields }
  TStringGridFields = class(TCollection)
  private
    FOwner: TDBAdvStringGrid;
    function GetItem(Index: Integer): TStringGridField;
    procedure SetItem(Index: Integer; const Value: TStringGridField);
  public
    constructor Create(aOwner:TDBAdvStringGrid);
  protected
    procedure Update(Item:TCollectionItem); override;
  public
    function Add:TStringGridField;
    function Insert(index:integer): TStringGridField;
    property Items[Index: Integer]: TStringGridField read GetItem write SetItem; default;
    function GetOwner: TPersistent; override;
    function FindField(const FieldName: string): TStringGridField;
  end;

  { TAdvGridDataLink }
  TAdvGridDataLink = class(TDataLink)
  private
    FGrid: TDBAdvStringGrid;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(distance:integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(AGrid: TDBAdvStringGrid);
    destructor Destroy; override;
  end;

  TDataMapMode = (dmRow,dmCell);

  { TDBAdvStringGrid }
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvStringGrid = class(TAdvStringGrid)
  private
    FMapping: array[0 .. MAX_FIELDS] of Integer;
    FDataLink: TAdvGridDataLink;
    FOldState: TDataSetState;
    FCellGraphic: TCellGraphic;
    FStringGridFields : TStringGridFields;
    FCloseWhenLoaded: Boolean;
    FLoaded: Boolean;
    FSelectScroll: Boolean;
    FDataScroll: Boolean;
    FKeepLinked: Boolean;
    FPageMode: Boolean;
    FHTMLTemplate: TStringList;
    FDataMapMode: TDataMapMode;
    FMaxRows: Integer;
    FLastRow,FLastCol: Integer;
    FWidths: TIntList;
    FFixedColumns: Integer;
    FShowPictureFields: Boolean;
    FShowMemoFields: Boolean;
    FShowBooleanFields: Boolean;
    FShowIndicator: Boolean;
    FPicture: TPicture;
    //FDeleting: Boolean;
    FDetailGrid: Boolean;
    FCurrCell: string;
    FInsLastRow: Boolean;
    FFieldColMoving: Boolean;
    FDataLoaded: TNotifyEvent;
    function GetDataSource: TDataSource;
    procedure SetDatasource(Value: TDatasource);
    procedure SetCloseWhenLoaded(Value: Boolean);
    procedure SetStringGridFields(value:tStringGridFields);
    function CheckDataSet: Boolean;
    function CheckDataSetRW: Boolean;
    function MaxVisibleRows:integer;
    function MaxRowsInDataset:integer;
    procedure SetBufferCount;
    function InitMapping: Boolean;
    procedure SetPageMode(const Value: boolean);
    procedure SetHTMLTemplate(const Value: TStringList);
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMVScroll(var WMScroll: TWMScroll); message WM_VSCROLL;
    procedure WMSize(var Msg:TWMSize); message WM_SIZE;
    procedure StoreColWidths;
    procedure ReStoreColWidths;
    procedure SetShowIndicator(const Value: boolean);
    function GetFixedColsEx: Integer;
    procedure SetFixedColsEx(const Value: Integer);
    procedure SetShowBooleanFields(const Value: Boolean);
    procedure SetShowMemoFields(const Value: Boolean);
    procedure SetShowPictureFields(const Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure ClearAll;
    procedure UpdateEditingCell(ACol,ARow: Integer; Value: string); override;
    procedure PasteInCell(ACol,ARow: Integer; Value: string); override;
    function GetCurrentCell: string; override;
    procedure SetCurrentCell(const AValue: string); override;
    function ToggleCheck(ACol,ARow: Integer; FromEdit: Boolean): Boolean; override;
    procedure Loaded; override;
    function  SelectCell(ACol, ARow: longint): Boolean; override;
    procedure RestoreCache; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetActive(Active: Boolean);
    procedure ShowFields;
    procedure GetCellAlign(ACol,ARow:Integer;var HAlign: TAlignment;var VAlign: TVAlignment); override;
    procedure ColumnMoved(FromIndex, ToIndex: Integer); override;
    function UpdateDataRow(ARow:integer): Boolean;
    function UpdateDataCell(ACol,ARow:integer): Boolean;
    procedure UpdateVisibleCells;
    procedure UpdateCursorPos;
    procedure UpdateRowCount;
    procedure TemplateChanged(Sender:TObject);
    procedure DrawCell(ACol,ARow:longint;ARect:TRect;AState:TGridDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoInsertRow(ARow: Integer); override;
    procedure DoDeleteRow(ARow: Integer); override;
    procedure GetDisplText(c,r: Integer; var Value: string); override;
    function GetCellType(ACol,ARow: Integer): TCellType; override;
    function GetCellGraphic(ACol,ARow: Integer): TCellGraphic; override;
    procedure GetCellColor(ACol,ARow: Integer;AState: TGridDrawState; ABrush: TBrush; AFont: TFont); override;
    procedure GetCellPrintColor(ACol,ARow: Integer;AState: TGridDrawState; ABrush: TBrush; AFont: TFont); override;
    procedure GetCellEditor(ACol,ARow: Integer;var AEditor:TEditorType); override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function ColumnField(Col: Integer): TField;
    function VisibleFieldCount: Integer;
    function VisibleFieldIndex(i: Integer): Integer;
    function GetHTMLTemplate: string;
    procedure QueryAddRow(var AllowAdd: Boolean); override;
    procedure QueryInsertRow(ARow: Integer; var AllowInsert: Boolean); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function CanEditShow: Boolean; override;
  public
    function ValidateCell(const NewValue:string): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; override;
    function GetVersionString: string; override;
    procedure LoadFromDataSource;
    procedure CalcFooter(ACol: Integer); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    property CloseWhenLoaded: Boolean read FCloseWhenLoaded write SetCloseWhenLoaded;
    property KeepLinked: Boolean read FKeepLinked write FKeepLinked;
    property DataMapMode: TDataMapMode read FDataMapMode write FDataMapMode;
    property DataSource: TDatasource read GetDatasource write SetDatasource;
    property DetailGrid: Boolean read FDetailGrid write FDetailGrid;
    property Fields: TStringGridFields read FStringGridFields write SetStringGridFields;
    property FixedCols: Integer read GetFixedColsEx write SetFixedColsEx;
    property PageMode: Boolean read FPageMode write SetPageMode;
    property HTMLTemplate: TStringList read FHTMLTemplate write SetHTMLTemplate;
    property ShowMemoFields: Boolean read FShowMemoFields write SetShowMemoFields;
    property ShowPictureFields: Boolean read FShowPictureFields write SetShowPictureFields;
    property ShowBooleanFields: Boolean read FShowBooleanFields write SetShowBooleanFields;
    property ShowIndicator: Boolean read FShowIndicator write SetShowIndicator;
    property OnDataLoaded: TNotifyEvent read FDataLoaded write FDataLoaded;
  end;

implementation

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;



function HTMLDBReplace(s:string;dataset:tdataset):string;
var
  beforetag,aftertag,fld,dbfld:string;
  i,j:integer;
begin
  beforetag:='';
  while Pos('<#',s) > 0 do
  begin
    i := pos('<#',s);
    beforetag := beforetag+copy(s,1,i-1); //part prior to the tag
    aftertag := copy(s,i,length(s)); //part after the tag
    j := pos('>',aftertag);
    fld := copy(aftertag,1,j-1);
    Delete(fld,1,2);
    Delete(s,1,i+j-1);

    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
        dbfld := DataSet.FieldByName(fld).DisplayText
      else
        dbfld := '('+fld+')';
    end
    else dbfld := '('+fld+')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;

function MinInt(a,b:integer):integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

procedure TDBAdvStringGrid.ClearAll;
begin
  // Remove all old rows and columns
  if ColumnHeaders.Count = 0 then
    Clear
  else
    ClearNormalCells;

  RowCount := FixedRows + 1;

  if (FixedCols > 0) and ShowIndicator then
    ColWidths[0] := 12;
end;

function TDBAdvStringGrid.MaxVisibleRows;
begin
  if FPageMode then
    Result := 2 + (Height div DefaultRowHeight)
  else
    Result := (Height div DefaultRowHeight) - 1;
end;

function TDBAdvStringGrid.CheckDataSet: Boolean;
begin
  Result := False;
  
  if not Assigned(FDataLink) then
    Exit;
    
  if not Assigned(FDataLink.DataSource) then
    Exit;
    
  if Assigned(FDataLink.Datasource.DataSet) then
  begin
    if FDataLink.Datasource.DataSet.Active then
      Result := True;
  end;
end;

function TDBAdvStringGrid.CheckDataSetRW: Boolean;
begin
  Result := False;

  if not Assigned(FDataLink) then
    Exit;

  if not Assigned(FDataLink.DataSource) then
    Exit;

  if Assigned(FDataLink.Datasource.DataSet) then
  begin
    if FDataLink.Datasource.DataSet.Active then
      Result := FDataLink.DataSource.DataSet.CanModify; 
  end;
end;

procedure TDBAdvStringGrid.GetCellAlign(ACol,ARow: Integer;var HAlign: TAlignment;var VAlign: TVAlignment);
var
  FCol: Integer;
  Fld: TField;
begin
  HAlign := taLeftJustify;

  if (ACol < FixedCols) then Exit;

  if Fields.Count > 0 then
  begin
    FCol := ACol - FixedCols;

    if  (FCol < Fields.Count)  then
    begin
      if Fields[FCol].FieldName <> '' then
        HAlign := Fields[FCol].Alignment;
    end;
  end
  else if CheckDataSet then
  begin
    Fld := ColumnField(ACol);

    if Assigned(Fld) then
      HAlign := Fld.Alignment;
  end;

  inherited;
end;


procedure TDBAdvStringGrid.ShowFields;
var
  i: Integer;
begin
  if FixedRows = 0 then
    Exit;

  if FHTMLTemplate.Count = 0 then
  begin
    if Fields.Count > 0 then
      ColCount := FixedCols + Fields.Count - NumHiddenColumns
    else if Assigned(FDataLink) then
    	if Assigned(FDataLink.DataSet) then
    		if FDataLink.DataSet.Active then
      		ColCount := FixedCols + VisibleFieldCount - NumHiddenColumns;
  end;

  for i := 1 to Fields.Count do
  begin
    if (Fields.Items[i - 1].Title <> '') then
      Cells[MinInt(1,FixedCols) + i - 1,0] := Fields.Items[i-1].Title
    else
      Cells[MinInt(1,FixedCols) + i - 1,0] := Fields.Items[i-1].FieldName;
  end;
end;


function TDBAdvStringGrid.UpdateDataCell(acol,arow:integer): Boolean;
var
  d:TDataSet;
  EmptyRow: Boolean;
  NewRecord,OldRecord: Integer;
  s: string;
  k: Integer;

begin
  {$IFDEF TMSDEBUG}
  DbgMsg('in update data cell');
  {$ENDIF}
  Result := False;
  EmptyRow := True;

  if (ARow < 0) or (ACol < 0) then
    Exit;

  if FHTMLTemplate.Count = 0 then
    Exit;

  if not CheckDataSet then
    Exit;

  d := FDataLink.Datasource.DataSet;
  if d.FieldCount = 0 then
    Exit;

  OldRecord := FDataLink.ActiveRecord;
  NewRecord := (ARow - FixedRows) * (ColCount - MinInt(1,FixedCols))+(ACol - (MinInt(1,FixedCols)));

  FDataLink.ActiveRecord := NewRecord;

  {$IFDEF TMSDEBUG}
  DbgPoint('update data cell : ',point(acol,arow));
  {$ENDIF}

  if FDataLink.ActiveRecord = NewRecord then
  begin
    with d do
    begin
      for k := 1 to d.FieldCount do
        if Trim(Fields[k - 1].AsString) <> '' then
          EmptyRow := False;

      if not EmptyRow then
      begin
        s := HTMLDBReplace(GetHTMLTemplate,d);
        if s <> Cells[ACol,ARow] then
          Cells[ACol,ARow] := s;
      end
      else
      begin
        if ARow < RowCount then
          Cells[ACol,ARow] := '';
      end;

    end;
  end;

  FDataLink.ActiveRecord := oldrecord;
  Result := EmptyRow;
end;


function TDBAdvStringGrid.UpdateDataRow(arow:integer): Boolean;
var
  d: TDataSet;
  j,k: Integer;
  dt: TFieldType;
  FieldIdx: Integer;
  mapped: Boolean;
  NumFields: Integer;
  oldrecord: Integer;
  field: TField;
  emptyrow: Boolean;
  s: string;

begin
  Result := False;
  EmptyRow := True;

  if FPageMode = True then
    Exit;

  if (ARow < 0) then
    Exit;

  if not CheckDataSet then
    Exit;

  d := FDataLink.Datasource.DataSet;
  
  if VisibleFieldCount = 0 then
    Exit;

  oldrecord := FDataLink.ActiveRecord;

  FDataLink.ActiveRecord := ARow;

  if (FDataLink.ActiveRecord = ARow) then
  begin
    ARow := ARow + FixedRows;
    if (ARow >= RowCount) then
      Exit;

    mapped := InitMapping;
    
    if mapped then
      NumFields := Fields.Count
    else
      NumFields := VisibleFieldCount;

    for k := 1 to Fields.Count do
    begin
      if (Fields.Items[k - 1].FFieldName <> '') then
      try
        Field := d.FieldByName(Fields.Items[k - 1].FFieldName);
        if Assigned(Field) then
          FMapping[k - 1] := Field.Index;
      finally
      end;
    end;

    with d do
    begin
      if (FHTMLTemplate.Count > 0) then
      begin
        for j := 1 to FHTMLTemplate.Count do
        begin
          s := HTMLDBReplace(FHTMLTemplate.Strings[j - 1],d);
          if s <> Cells[j + MinInt(1,FixedCols) - 1, arow] then
            Cells[j + MinInt(1,FixedCols) - 1, ARow] := s;
        end;

        for k := 1 to VisibleFieldCount do
        begin
          if (Trim(Fields[VisibleFieldIndex(k - 1)].AsString) <> '') then
            EmptyRow := False;
        end;
      end
      else
        for j := 1 to NumFields do
        begin
          if mapped then
            FieldIdx := VisibleFieldIndex(FMapping[j - 1])
          else
            FieldIdx := VisibleFieldIndex(j - 1);

          if (FieldIdx = -1) then
            Continue;

          dt := Fields[FieldIdx].DataType;

          s := '';

          if not (dt in [ftBlob,ftMemo,ftGraphic,ftTypedBinary]) then
            s := Trim(Fields[FieldIdx].DisplayText);

          if ((dt in [ftMemo,ftFmtMemo,ftOraCLOB]) or ((dt = ftBlob) and ((Fields[FieldIdx] as TBlobField).BlobType = ftMemo))) and FShowMemoFields then
            s := Trim(Fields[FieldIdx].AsString);

          if (dt = ftBoolean) and FShowBooleanFields then
            SetCheckBoxState(j + MinInt(1,FixedCols) - 1,ARow,Fields[FieldIdx].AsBoolean)
          else
            if (s <> Cells[j + MinInt(1,FixedCols) - 1,ARow]) then Cells[j+MinInt(1,FixedCols)-1, ARow] := s;

          for k := 1 to VisibleFieldCount do
            if (Trim(Fields[VisibleFieldIndex(k - 1)].AsString) <> '') then
              EmptyRow := False;
        end;
    end;
  end;

  FDataLink.ActiveRecord := oldrecord;
  Result := emptyrow;
end;


procedure TDBAdvStringGrid.SetBufferCount;
begin
  if FPageMode then
    Exit;

  if FDataMapMode = dmRow then
    FDataLink.BufferCount := RowCount - FixedRows
  else
    FDataLink.BufferCount := (RowCount - FixedRows) * (ColCount-MinInt(1,FixedCols));
end;

function TDBAdvStringGrid.MaxRowsInDataset: Integer;
var
  cb: TBookMark;
  ol: Boolean;
  iseof,isbof: Boolean;
begin
  FMaxRows := -1;
  Result := -1;
  if not CheckDataSet then
    Exit;

  ol := FKeepLinked;
  FKeepLinked := False;
  FDataScroll := True;
  FDataLink.DataSet.DisableControls;

  iseof := FDataLink.DataSet.Eof;
  isbof := FDataLink.DataSet.Bof;

  if isbof and iseof then
  begin
    Result := 0;
    FMaxRows := 0;
  end
  else
    with FDataLink.DataSet do
    begin
      cb := GetBookMark;
      First;
      Result := MoveBy($7FFFFFFF) + 1;

      GotoBookMark(cb);
      FreeBookMark(cb);

      FMaxRows := Result;
    end;

  if iseof then
    FDataLink.DataSet.Next;

  if isbof then
    FDataLink.DataSet.Prior;

  if FDataLink.DataSet.State = dsInsert then
    Result := Result + 1;

  FDataLink.DataSet.EnableControls;
  FDataScroll := False;
  FKeepLinked := ol;
end;

procedure TDBAdvStringGrid.UpdateRowCount;
var
  rc: Integer;
  flg: Boolean;
begin
  rc := MaxRowsInDataset;

  flg := rc+ FixedRows <> RowCount;

  if FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow) then
    inc(rc);

  if rc >= 0 then
  begin
    if FDataMapMode = dmRow then
    begin
      if FixedRows + rc <> RowCount then
       begin
        if rc = 0 then inc(rc);
        RowCount := FixedRows + rc;
       end;
    end
    else
    begin
      if (FixedRows + rc <> RowCount) then
        RowCount := FixedRows + 1 + (rc div (ColCount - MinInt(1,FixedCols)));
    end;

    if PageMode then
    begin
      FDataLink.BufferCount := rc;
    end
    else
    begin
      if FDataLink.BufferCount <> rc then
        FDataLink.BufferCount := rc;
    end;
  end;

  if flg then
  begin
    Invalidate;
  end;
  InitSortXRef;
end;

procedure TDBAdvStringGrid.UpdateCursorPos;
var
 NewRow, NewCol:integer;

begin
  Exit;

  FDataScroll := True;
  if FDataMapMode = dmRow then
  begin
    if (FDataLink.DataSet.Bof) then
      NewRow := FixedRows
    else
     if (fDataLink.DataSet.Eof) then
       NewRow := RowCount-1
     else
     begin
       if (FDataLink.DataSet.State = dsInsert) then
         NewRow := RowCount - 1
       else
         NewRow := FDataLink.ActiveRecord + FixedRows;
     end;
     if (NewRow <> Row) then
       Row := NewRow;
  end
  else
  begin
    NewRow := Row;
    NewCol := Col;
    if (FDataLink.DataSet.State = dsInsert) then
    begin
      if not FPageMode then
        NewRow := ((FDataLink.BufferCount + 1) div (ColCount - MinInt(1,FixedCols))) + FixedRows;
      if not FPageMode then
        NewCol := ((FDataLink.BufferCount + 1) mod (ColCount - MinInt(1,FixedCols))) + MinInt(1,FixedCols);
    end
    else
    begin
      NewRow := (FDataLink.ActiveRecord div (ColCount - FixedCols)) + FixedRows;
      NewCol := (FDataLink.ActiveRecord mod (ColCount - MinInt(1,FixedCols))) + MinInt(1,FixedCols);
    end;

    if (NewRow <> Row) then
      Row := NewRow;
    if (NewCol <> Col) then
      Col := NewCol;
  end;

  FDataScroll := False;
end;

procedure TDBAdvStringGrid.UpdateVisibleCells;
var
  i,j: Integer;
begin
  if (FDataMapMode = dmRow) then
  begin
    j := TopRow + VisibleRowCount - 1;
    if (j >= RowCount) then
      j := RowCount;
    i := TopRow - FixedRows;

    while (i <= j) and (i < RowCount) and (i < FDataLink.BufferCount) do
    begin
      if UpdateDataRow(i) then
        if (FDataLink.DataSet.State = dsBrowse) then
          if (RowCount > FixedRows + 1) then
            RowCount := RowCount - 1;
      Inc(i);
    end;
  end
  else
  begin
    i := (TopRow - FixedRows) * (ColCount - MinInt(1,FixedCols));
    j := (TopRow + VisibleRowCount-1) * (ColCount - MinInt(1,FixedCols));

    while (i <= j) and (i < FDataLink.BufferCount) do
    begin
      if UpdateDataCell(MinInt(1,FixedCols)+(i mod (ColCount - MinInt(1,FixedCols))),FixedRows + (i div (ColCount-MinInt(1,FixedCols)))) then
      begin
        if (FDataLink.DataSet.State = dsBrowse) and (i mod (ColCount - MinInt(1,FixedCols)) = 0) then
          if (RowCount > FixedRows + 1) then
            RowCount := RowCount - 1;
      end;
      Inc(i);
    end;
  end;
end;

function TDBAdvStringGrid.InitMapping: Boolean;
var
  i: Integer;
begin
  for i := 0 to MAX_FIELDS do FMapping[i] := i;
  Result := Fields.Count > 0;
end;

procedure TDBAdvStringGrid.LoadFromDataSource;
var
  d:TDataSet;
  i,j:integer;
  dt:TFieldType;
  cb:TBookMark;
  field:TField;
  mapped: Boolean;
  numfields:integer;
  fieldidx:integer;

begin
  dt := ftUnknown;


  // Check to see if Dataset is Open
  if not CheckDataSet then
  begin
    if (Fields.Count > 0) then ShowFields;
    Exit;
  end;

  if FPageMode then
  begin
    if Fields.Count > 0 then
      ColCount := FixedCols + Fields.Count
    else
      ColCount := FixedCols + VisibleFieldCount;
    Exit;
  end;


  // Remove all old rows and columns
  StoreColWidths;
  ClearAll;

  d := FDataLink.Datasource.DataSet;

  with d do
  begin
    DisableControls;
    cb := GetBookMark;

    // mapping between columns & fields
    mapped := InitMapping;
    
    if  mapped then
      Numfields := Self.Fields.Count
    else
      Numfields := FieldCount;

    if FDataMapMode = dmRow then
    begin
      if (FHTMLTemplate.Count = 0) then
        ColCount := FixedCols + NumFields
      else
        ColCount := FixedCols + FHTMLTemplate.Count;
    end;

    for i := 1 to Self.Fields.Count do
    begin
      if (Self.Fields.Items[i - 1].FFieldName <> '') then
      try
        Field := FieldByName(Self.Fields.Items[i-1].FFieldName);
        if Assigned(Field) then FMapping[i-1] := Field.Index;
      finally
      end;
    end;

    First;

    // if Fixed Rows = 0 and UseHeader True, Iterate through Fields and
    // assign Column Labels

    if (FixedRows = 1) and (FHTMLTemplate.Count = 0) then
    begin
      for i := 1 to numfields do
      begin
        if mapped then
          FieldIdx := VisibleFieldIndex(FMapping[i - 1])
        else
          FieldIdx := VisibleFieldIndex(i - 1);

        if FieldIdx = -1 then
          Continue;

        if mapped then
        begin
          if (self.Fields.Items[i - 1].Title <> '') then
            Cells[i+MinInt(1,FixedCols) - 1,0] := self.Fields.Items[i-1].Title;
        end
        else
        begin
          if Length(TField(Fields[FieldIdx]).DisplayLabel) > 0 then
          begin
            dt := Fields[FieldIdx].DataType;
            if not (dt in [ftBlob,ftTypedBinary]) then
              Cells[i + MinInt(1,FixedCols)-1,0] := TField(Fields[fieldidx]).DisplayLabel;
          end
          else
          begin
            if not (dt in [ftBlob,ftTypedBinary]) then
              Cells[i+MinInt(1,FixedCols)-1,0] := TField(Fields[fieldidx]).DisplayName;
          end;  // if Length(TField(...
        end;
      end;  // for i := 0 to FieldCount-1
    end;  // (a = 0) and (FUseHeader = True)

    // Iterate through records and assign to rows
    i := 0;
    BeginUpdate;

    while (not Eof) and ((RowCount < MaxVisibleRows) or (FPageMode = False)) do
    begin
      // Iterate through the fields array and put data in cells

      if FDataMapMode = dmRow then
      begin
        if FHTMLTemplate.Count > 0 then
        begin
          for j := 1 to FHTMLTemplate.Count do
           Cells[j+MinInt(1,FixedCols)-1, RowCount-1] := HTMLDBReplace(FHTMLTemplate.Strings[j-1],d);
        end
        else
        begin
          for j:=1 to Numfields do
          begin
            if mapped then
              FieldIdx := VisibleFieldIndex(FMapping[j - 1])
            else
              FieldIdx := VisibleFieldIndex(j - 1);

            if FieldIdx = -1 then
              Continue;

            dt := Fields[FieldIdx].DataType;
            
            if not (dt in [ftBlob,ftMemo,ftGraphic,ftTypedBinary,ftBoolean]) then
               Cells[j + MinInt(1,FixedCols) - 1, RowCount-1] := Trim(Fields[FieldIdx].DisplayText);

            if (dt = ftGraphic) and FShowPictureFields then
              begin
               with CreatePicture(j+MinInt(1,FixedCols) - 1,RowCount - 1,True,StretchWithAspectRatio,4,haLeft,vaTop) do
                 Assign(Fields[FieldIdx]);
              end;

            if (dt in [ftMemo,ftFmtMemo,ftOraCLOB]) and FShowMemoFields then
              begin
                Cells[j + MinInt(1,FixedCols)-1, RowCount - 1] := Trim(Fields[FieldIdx].AsString);
              end;

            if dt = ftBoolean then
             if FShowBooleanFields then
                AddCheckBox(j+MinInt(1,FixedCols)-1, RowCount - 1,Fields[FieldIdx].AsBoolean, False)
             else
                Cells[j + MinInt(1,FixedCols) - 1, RowCount - 1] := Trim(Fields[FieldIdx].DisplayText);
          end;
        end;
        Next;
        if not Eof then
          RowCount := RowCount + 1;
      end
      else
      begin
        if FHTMLTemplate.Count > 0 then
        begin
          Cells[i + MinInt(1,FixedCols), RowCount-1] := HTMLDBReplace(GetHTMLTemplate,d);
        end;  

        Next;
        Inc(i);
        if (i = ColCount - MinInt(1,FixedCols)) and not Eof then
        begin
          i := 0;
          RowCount := RowCount + 1;
        end;
      end;
    end;  // while not Eof

    EndUpdate;

    GotoBookMark(cb);
    FreeBookMark(cb);

    if (FCloseWhenLoaded) and not (csDesigning in ComponentState) then Close;

    SetBufferCount;
    MaxRowsInDataset;
    EnableControls;
  end;

  Row := FixedRows;

  RestoreColWidths;

  FLoaded := True;

  if Assigned(OnDataLoaded) then
    OnDataLoaded(Self);
end;

procedure TDBAdvStringGrid.SetDataSource(Value : TDatasource);
var
  cb: TBookmark;
  i: Integer;
begin
  if not Assigned(FDataLink) then
    Exit;

  if (Value = nil) then
  begin
     SetActive(False);
     ClearAll;
  end;

  if (FDataLink.DataSource <> Value) then
  begin
     FDataLink.DataSource := Value;


     if CheckDataset then
     with FDataLink.DataSet do
     begin
       cb := GetBookMark;
       i := MoveBy(-$7FFFFFFF);

       GotoBookMark(cb);
       FreeBookMark(cb);

       Row := -i + 1;

     end;
  end;

  if (Value <> Nil) then Value.FreeNotification(Self);

  if not Assigned(FDataLink.DataSource) then
    ClearAll;
end;

function TDBAdvStringGrid.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TDBAdvStringGrid.SetCloseWhenLoaded(Value: Boolean);
begin
  if FCloseWhenLoaded <> Value then
    FCloseWhenLoaded := Value;
end;

procedure TDBAdvStringGrid.SetStringGridFields(value:tStringGridFields);
begin
  FStringGridFields.Assign(value);
end;

function TDBAdvStringGrid.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TDBAdvStringGrid.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)))+' '+DATE_VER;
end;

constructor TDBAdvStringGrid.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(aOwner);
  for i := 0 to MAX_FIELDS do
    FMapping[i] := i;
  FDataLink := TAdvGridDataLink.Create(self);
  FDataLink.VisualControl := True;
  FStringGridFields := TStringGridFields.Create(self);
  FLoaded := False;
  FKeepLinked := True;
  FHTMLTemplate := TStringList.Create;
  FHTMLTemplate.OnChange := TemplateChanged;
  FDataMapMode := dmRow;
  FShowPictureFields := True;
  FShowMemoFields := True;
  FShowIndicator := True;
  FWidths := TIntList.Create(0,0);
  FPageMode := True;
  FVirtualCells := True;
  FCellGraphic := TCellGraphic.Create;
  FPicture := TPicture.Create;
end;

destructor TDBAdvStringGrid.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FStringGridFields.Free;
  FHTMLTemplate.Free;
  FWidths.Free;
  FCellGraphic.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TDBAdvStringGrid.Loaded;
begin
  inherited Loaded;
  
  if not FLoaded and not FPageMode then
    LoadFromDataSource
  else
    ShowFields;  
end;

procedure TDBAdvStringGrid.SetActive(Active: boolean);
begin
  if not Active then
  begin
    ClearAll;
    if FHTMLTemplate.Count = 0 then
    begin
      Clear;
    end
    else
    begin
      ClearNormalCells;
    end;
  end
  else
  begin
    if not (csLoading in ComponentState) and not FPageMode then
      LoadFromDatasource;
  end;
end;



{ FGridDataLink }

procedure TAdvGridDataLink.ActiveChanged;
begin
  inherited ActiveChanged;

  {$IFDEF TMSDEBUG}
  DbgMsg('active changed');
  {$ENDIF}

  if Assigned(FGrid) and Assigned(DataSet) then
  begin
    with FGrid do
    begin
      SetActive(Dataset.Active);
      UpdateRowCount;
    end;
  end;

  if Assigned(DataSet) then
  begin
    if DataSet.Active then
    begin
      with FGrid do
      begin
        { runtime field collection creation
        if (Fields.Count = 0) and (not (csDesigning in ComponentState)) then
        begin
          bRTFields := True;
          for i := 0 to DataSet.FieldCount-1 do
          with Fields.Add do
          begin
            FFieldName := DataSet.Fields[i].FieldName;
            FTitle     := DataSet.Fields[i].DisplayLabel;
          end
        end;
        }

        if Fields.Count > 0 then
          ColCount := FixedCols + Fields.Count - NumHiddenColumns
        else
          ColCount := FixedCols + VisibleFieldCount - NumHiddenColumns;

        InitMapping;

        // Set Column Widths if runtime fields - Jan 2003 ...
        {
        if bRTFields then // and (not FGrid.ColumnSize.Save) does not work!
        begin
          if (FixedCols > 0) and ShowIndicator then
          begin
            ofs := 1;
            ColWidths[0] := 12
          end
          else
            ofs := 0;
          nScaling     := 7.25; // Needs attention & separate one for Title font
          nMaxColWidth :=20 * nScaling;  // needs to be property
          for i := 0 to FGrid.Fields.Count -1 do
          begin
            m := FGrid.FMapping[i];
            case DataSet.Fields[m].DataType of
              ftBlob, ftGraphic, ftParadoxOle, ftDBaseOle, ftTypedBinary:
                  nWidth := Max(5 * nScaling,
                              Length(FGrid.Fields[m].FTitle)* nScaling);
              ftMemo,ftFmtMemo:
                  if FGrid.FShowMemoFields then
                    nWidth := nMaxColWidth
                  else
                    nWidth := Max(5 * nScaling,
                              Length(FGrid.Fields[m].FTitle)* nScaling);
            else // case
              nWidth :=
                  Max(Min(DataSet.Fields[m].DisplayWidth * nScaling,nMaxColWidth),
                      Length(FGrid.Fields[m].FTitle)* nScaling);
            end;
            ColWidths[i+ofs] := Trunc(nWidth);
          end
        end;
        }

      end;
      FGrid.ShowFields;
    end;
  end
  else
  begin
    //FGrid.Fields.Clear;
  end;
  FGrid.Invalidate;
end;

constructor TAdvGridDataLink.Create(AGrid: TDBAdvStringGrid);
begin
  inherited Create;
  FGrid := Agrid;
end;

procedure TAdvGridDataLink.LayoutChanged;
begin
  if not FGrid.FKeepLinked then Exit;

  {$IFDEF TMSDEBUG}
  DbgMsg('layout changed');
  {$ENDIF}

  inherited;

  with FGrid do
  begin
    if PageMode then
    begin
      if Fields.Count > 0 then
        ColCount := FixedCols + Fields.Count - NumHiddenColumns
      else
        ColCount := FixedCols + VisibleFieldCount - NumHiddenColumns;
    end;
  end;

  FGrid.Invalidate;
end;


procedure TAdvGridDataLink.DataSetChanged;
var
  NewRow,NewCol,NumRows: Integer;
label
  Skip;
begin
  {$IFDEF TMSDEBUG}
  case DataSet.State of
  dsInactive:  DbgMsg('datasetchanged inactive');
  dsBrowse:  DbgMsg('datasetchanged browse');
  dsEdit:   DbgMsg('datasetchanged edit');
  dsInsert:   DbgMsg('datasetchanged insert');
  dsSetKey :  DbgMsg('datasetchanged setkey');
  dsCalcFields:   DbgMsg('datasetchanged calcfields');
  dsFilter:   DbgMsg('datasetchanged filter');
  dsNewValue:   DbgMsg('datasetchanged newvalue');
  dsOldValue:   DbgMsg('datasetchanged oldvalue');
  dsCurValue:   DbgMsg('datasetchanged curvalue');
  dsBlockRead:   DbgMsg('datasetchanged blockread');
  dsInternalCalc:   DbgMsg('datasetchanged internalcalc');
  dsOpening:   DbgMsg('datasetchanged opening');
  end;
  {$ENDIF}

  NumRows := FGrid.FMaxRows;

  if not FGrid.FKeepLinked then
    Exit;

  inherited;

  if (DataSet.State <> dsEdit) and (FGrid.FOldState = dsEdit) then
    FGrid.HideInplaceEdit;

  if (DataSet.State = dsBrowse) and (FGrid.FOldState = dsInsert) then
  begin
    FGrid.HideInplaceEdit;
    // do full update when coming from insert
    {$IFDEF TMSDEBUG}
    DbgMsg('switch from insert mode');
    {$ENDIF}
    FGrid.Invalidate;
  end;

  if (DataSet.State = dsBrowse) then
    FGrid.UpdateRowCount;

  // DB insert state change
  if (DataSet.State = dsInsert) and (FGrid.FOldState <> dsInsert) then
  begin
    // do full update when going to insert
    {$IFDEF TMSDEBUG}
    DbgMsg('switch to insert mode');
    {$ENDIF}
    FGrid.Invalidate;
  end;

  FGrid.KeepLinked := False;
  FGrid.FDataScroll := True;
  FGrid.FSelectScroll := True;

  if DataSet.Bof then
    FGrid.Row := FGrid.FixedRows;

  if DataSet.Eof then
    FGrid.Row := FGrid.RowCount - 1;

  if not DataSet.Bof and not DataSet.Eof then

    if FGrid.FDataMapMode <> dmRow then
    begin //goto last open cell?
      if DataSet.State <> dsInsert then
      begin
        NewRow := (ActiveRecord div (FGrid.ColCount - FGrid.FixedRightCols - MinInt(1,FGrid.FixedCols))) + FGrid.FixedRows;
        NewCol := (ActiveRecord mod (FGrid.ColCount - FGrid.FixedRightCols - MinInt(1,FGrid.FixedCols))) + MinInt(1,FGrid.FixedCols);
        if NewRow <> FGrid.Row then
          FGrid.Row := NewRow;
        if NewCol <> FGrid.Col then
          FGrid.Col := NewCol;
      end;
    end
    else
    begin
      if DataSet.State <> dsInsert then
      begin
        if ActiveRecord + FGrid.FixedRows < FGrid.RowCount then
          FGrid.Row := ActiveRecord + FGrid.FixedRows
        else
          FGrid.Row := FGrid.RowCount - 1;

        if goRowSelect in FGrid.Options then
          FGrid.Selection := TGridRect(Rect(FGrid.FixedCols,FGrid.Row,FGrid.ColCount - 1, FGrid.Row));
      end;
    end;

  //after insert, update rowcount

  {$IFDEF TMSDEBUG}
  DbgMsg('state change:'+inttostr(integer(DataSet.State))+':'+inttostr(integer(FGrid.FoldState)));
  {$ENDIF}

  // switch to insert state
  if ((DataSet.State <> FGrid.FOldState) and (DataSet.State = dsInsert) and not FGrid.PageMode) then
  begin
    if (FGrid.DataMapMode = dmRow) then
    begin
      if (FGrid.MaxVisibleRows >= FGrid.RowCount) then
      begin
        if not FGrid.FPageMode then BufferCount := BufferCount + 1;
        if not FGrid.FPageMode then FGrid.RowCount := FGrid.RowCount + 1;
        FGrid.Row := FGrid.RowCount - 1;
        if goRowSelect in FGrid.Options then
          FGrid.Selection := TGridRect(Rect(FGrid.FixedCols,FGrid.Row,FGrid.ColCount - 1, FGrid.Row));
        FGrid.UpdateVisibleCells;
      end;
    end
    else
    begin
      if not FGrid.FPageMode then
        BufferCount := BufferCount + 1;

      NewRow := FGrid.Row;
      NewCol := FGrid.Col;

      if not FGrid.FPageMode then
        NewRow := ((FGrid.FMaxRows) div (FGrid.ColCount-MinInt(1,FGrid.FixedCols))) + FGrid.FixedRows;
      if not FGrid.FPageMode then
        NewCol := ((FGrid.FMaxRows) mod (FGrid.ColCount - FGrid.FixedRightCols -MinInt(1,FGrid.FixedCols))) + MinInt(1,FGrid.FixedCols);

      if not FGrid.FPageMode and (NewRow>FGrid.RowCount-1) then
        FGrid.RowCount := FGrid.RowCount + 1;

      if (NewRow <> FGrid.Row) then
        FGrid.Row := NewRow;

      if (NewCol <> FGrid.Col) then
        FGrid.Col := NewCol;

      if goRowSelect in FGrid.Options then
        FGrid.Selection := TGridRect(Rect(FGrid.FixedCols,FGrid.Row,FGrid.ColCount - 1 - FGrid.FixedRightCols, FGrid.Row));
      FGrid.UpdateVisibleCells;
    end;
  end;

  if ((DataSet.State <> FGrid.FOldState) and (DataSet.State = dsInsert) and FGrid.PageMode) then
  begin
    // for an empty dataset, do not increase rows
    if NumRows > 0 then
      FGrid.RowCount := FGrid.RowCount + 1;
    FGrid.FDataLink.BufferCount := FGrid.RowCount - FGrid.FixedRows;
    FGrid.Row := FGrid.FDataLink.ActiveRecord + FGrid.FixedRows;
  end;

Skip:

  if (FGrid.DetailGrid) and not (DataSet.State = dsInsert) then
  begin
    FGrid.UpdateRowCount;
    FGrid.UpdateVisibleCells;
    FGrid.Invalidate;
  end
  else
  begin
    if FGrid.PageMode then
      FGrid.UpdateVisibleCells
    else
      FGrid.RepaintRow(FGrid.Row);
  end;

  // switch from insert state to browse
  if ((DataSet.State <> FGrid.FOldState) and (FGrid.FOldState = dsInsert)) and not FGrid.FPageMode then
  begin
    if (FGrid.DataMapMode = dmRow) then
    begin
      FGrid.Row := ActiveRecord + FGrid.FixedRows;
      if goRowSelect in FGrid.Options then
        FGrid.Selection := TGridRect(Rect(FGrid.FixedCols,FGrid.Row,FGrid.ColCount - 1, FGrid.Row));
    end
    else
    begin
      NewRow := (ActiveRecord div (FGrid.ColCount-MinInt(1,FGrid.FixedCols))) + FGrid.FixedRows;
      NewCol := (ActiveRecord mod (FGrid.ColCount-MinInt(1,FGrid.FixedCols))) + MinInt(1,FGrid.FixedCols);
      if (NewRow <> FGrid.Row) then FGrid.Row := NewRow;
      if (NewCol <> FGrid.Col) then FGrid.Col := NewCol;
    end;
  end;

  if ((DataSet.State <> FGrid.FOldState) and (FGrid.FOldState in [dsInsert,dsEdit])) and FGrid.FPageMode then
  begin
    if not ((FGrid.FOldState = dsInsert) and (DataSet.State = dsEdit)) then
    begin
      FGrid.UpdateRowCount;
      FGrid.Row := ActiveRecord + FGrid.FixedRows;
      FGrid.InitSortXRef;
    end;
  end;

  FGrid.FSelectScroll := False;
  FGrid.FDataScroll := False;
  FGrid.FOldState := DataSet.State;
  FGrid.UpdateVisibleCells;
  FGrid.KeepLinked := True;
end;

procedure TAdvGridDataLink.DataSetScrolled(Distance:Integer);
var
  NewRow: Integer;
begin
  {$IFDEF TMSDEBUG}
  DbgInt('datasetscrolled',Distance);
  {$ENDIF}

  inherited DataSetScrolled(Distance);

  if not FGrid.FKeepLinked then Exit;
  if FGrid.FSelectScroll then Exit;

  FGrid.FDataScroll := True;
  FGrid.FSelectScroll := True;

  NewRow := ActiveRecord + FGrid.FixedRows;
  if (NewRow < FGrid.FixedRows) then
     NewRow := FGrid.FixedRows;
  if (NewRow >= FGrid.RowCount) then
    NewRow := FGrid.RowCount - 1;

  if FGrid.PageMode then
    FGrid.Row := FGrid.UnSortedRowIndex(NewRow)
  else
    FGrid.Row := NewRow;

  if goRowSelect in FGrid.Options then
    FGrid.Selection := TGridRect(Rect(FGrid.FixedCols,FGrid.Row,FGrid.ColCount - 1 -  FGrid.FixedRightCols, FGrid.Row));

  //FGrid.UpdateSelect;
  FGrid.FDataScroll := False;
  FGrid.FSelectScroll := False;
end;

procedure TAdvGridDataLink.RecordChanged(Field: TField);
begin
  {$IFDEF TMSDEBUG}
  DbgMsg('record changed');
  {$ENDIF}

  inherited RecordChanged(Field);

  with FGrid do
  begin
    if not FKeepLinked then Exit;

    if FGrid.PageMode then
      FGrid.RepaintRow(FGrid.Row)
    else
    begin
      if FDataMapMode = dmRow then
        UpdateDataRow(ActiveRecord)
      else
        UpdateDataCell(MinInt(1,FixedCols) + (ActiveRecord mod (ColCount-MinInt(1,FixedCols))),FixedRows+(ActiveRecord div (ColCount-MinInt(1,FixedCols))));
    end;

    if FGrid.FloatingFooter.Visible then
    begin
      FGrid.CalcFooter(-1);
    end;

    UpdateVisibleCells;
  end;

end;

destructor TAdvGridDataLink.Destroy;
begin
  inherited Destroy;
end;

{ TStringGridFields }

function TStringGridFields.Add: TStringGridField;
begin
  Result := TStringGridField(inherited Add);
  Update(Result);
end;

constructor TStringGridFields.Create(aOwner: TDBAdvStringGrid);
begin
  inherited Create(TStringGridField);
  FOwner := AOwner;
end;

function TStringGridFields.FindField(
  const FieldName: string): TStringGridField;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if AnsiCompareText(Result.FFieldName, FieldName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TStringGridFields.GetItem(Index: Integer): TStringGridField;
begin
  Result := TStringGridField(inherited GetItem(Index));
end;

function TStringGridFields.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TStringGridFields.Insert(index: integer): TStringGridField;
begin
  Result := TStringGridField(inherited Insert(index));
end;

procedure TStringGridFields.SetItem(Index: Integer;
  const Value: TStringGridField);
begin
  inherited SetItem(Index, Value);
end;

procedure TStringGridFields.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  with (GetOwner as TDBAdvStringGrid) do
  begin
    if not (csLoading in ComponentState) then
    begin
      LoadFromDataSource;
      Invalidate;
    end;
  end;
end;

{ TStringGridField }

procedure TStringGridField.Assign(Source: TPersistent);
begin
  FAlignment := (Source as TStringGridField).Alignment;
  FFieldName := (Source as TStringGridField).FieldName;
  FTitle := (Source as TStringGridField).Title;
  FColor := (Source as TStringGridFIeld).Color;
  FFont.Assign((Source as TStringGridFIeld).Font);
  FEditor := (Source as TStringGridField).Editor;
  FShowBands := (Source as TStringGridField).ShowBands;
  FEditMask := (Source as TStringGridField).EditMask;
  FReadOnly := (Source as TStringGridField).ReadOnly;
  FTag := (Source as TStringGridField).Tag;
  FComboItems.Assign((Source as TStringGridField).ComboItems);
  FSpinMin := (Source as TStringGridField).SpinMin;
  FSpinMax := (Source as TStringGridField).SpinMax;
  FSpinStep := (Source as TStringGridField).SpinStep;
  FEditLink := (Source as TStringGridField).EditLink;
end;

procedure TStringGridField.Changed;
begin
  with (Collection as TStringGridFields) do
  begin
    with (GetOwner as TDBAdvStringGrid) do
    begin
      if (csDesigning in ComponentState) and
         not (csLoading in ComponentState) then
        LoadFromDataSource;

      Invalidate;
    end;
  end;
end;

constructor TStringGridField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Color := clWindow;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FComboItems := TStringList.Create;
  FShowBands := True;
  FSpinStep := 1;
end;

destructor TStringGridField.Destroy;
begin
  FFont.Free;
  FComboItems.Free;
  inherited Destroy;
end;

procedure TStringGridField.FontChanged(Sender: TObject);
begin
  with (Collection as TStringGridFields) do
  begin
    with (GetOwner as TDBAdvStringGrid) do
      Invalidate;
  end;
end;

function TStringGridField.GetDisplayName: string;
begin
  Result := FFieldName;
end;

procedure TStringGridField.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    with (Collection as TStringGridFields) do
    begin
      with (GetOwner as TDBAdvStringGrid) do
        Invalidate;
    end;
  end;
end;

procedure TStringGridField.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    with (Collection as TStringGridFields) do
    begin
      with (GetOwner as TDBAdvStringGrid) do
        Invalidate;
    end;
  end;
end;

procedure TStringGridField.SetComboItems(const Value: TStringList);
begin
  FComboItems.Assign(Value);
end;

procedure TStringGridField.SetFieldName(const Value: string);
begin
  FFieldName := Value;
  Changed;
end;

procedure TStringGridField.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TStringGridField.SetShowBands(const Value: Boolean);
begin
  FShowBands := Value;
  Changed;
end;

procedure TStringGridField.SetTitle(const Value: string);
begin
  FTitle := Value;
  with (Collection as TStringGridFields) do
  begin
    with (GetOwner as TDBAdvStringGrid) do
    begin
      if  not (csLoading in ComponentState) then
      begin
        LoadFromDataSource;
        Invalidate;
      end;
    end;
  end;
end;         

function TDBAdvStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  d: TDataSet;
  Distance: Integer;
begin

  if CheckDataSet then
  begin
    d := FDataLink.Datasource.DataSet;

//    d.DisableControls;
    if FPageMode and not FSelectScroll then
    begin
      FSelectScroll := True;
      Distance := SortedRowIndex(ARow) - SortedRowIndex(Row);
      if Distance <> 0 then
        d.MoveBy(Distance);
      FSelectScroll := False;

      Result := inherited SelectCell(ACol,ARow);

      if FixedCols > 0 then
      begin
        RepaintCell(0,ARow);
        RepaintCell(0,Row);
      end;
      Exit;
    end;

//    d.EnableControls;

    if FLoaded and not FDataScroll and FKeepLinked then
    begin
      FKeepLinked := False;
      if FDataMapMode = dmRow then
      begin
        if (ARow >= FixedRows) and (ARow < RowCount) then
        begin
          FSelectScroll := True;
          Distance := ARow - Row;
          if Distance <> 0 then
            d.MoveBy(Distance);
          FSelectScroll := False;
        end;
      end
      else
      begin
        if (Cells[ACol,ARow] <> '') then
        begin
          FSelectScroll := True;
          Distance := (ARow-FLastrow)*(ColCount-MinInt(1,FixedCols))+(acol-flastcol);
          if Distance <> 0 then
            d.MoveBy(Distance);
          FSelectScroll := False;
          FLastCol := ACol;
          FLastRow := ARow;
        end;
      end;
      FKeepLinked := True;
    end;
  end;

  if FixedCols > 0 then
    RepaintCell(0,Row);

  Result := inherited SelectCell(Acol,Arow);

  if FixedCols > 0 then
    RepaintCell(0,ARow);

end;

procedure TDBAdvStringGrid.SetPageMode(const Value: boolean);
begin
  if FPageMode <> Value then
  begin
    FPageMode := Value;
    FVirtualCells := Value;
    if not (csLoading in ComponentState) then
    begin
      if FPageMode and (Rowcount > MaxVisibleRows) then
        RowCount := MaxVisibleRows;
    end;
    Invalidate;
  end;
end;

procedure TDBAdvStringGrid.WMKeyDown(var Msg:TWMKeydown);
var
  d: TDataSet;
  Distance,ColMove: Integer;

begin
  if CheckDataSet and FPageMode then
  begin
    d := FDataLink.Datasource.DataSet;

    if (FDataMapMode <> dmRow) then
    begin
      case msg.CharCode of
      VK_UP:
         if (Row = FixedRows) then
           d.MoveBy(-(Colcount-MinInt(1,FixedCols)));
      VK_DOWN:
        begin
          if (Row = MaxVisibleRows - 1) then
          begin
            ColMove := (ColCount - Col);
            Distance := (ColMove + (Colcount-MinInt(1,FixedCols))-1);
            {$IFDEF TMSDEBUG}
            DbgMsg('keydown move from : '+inttostr(FDataLink.ActiveRecord)+':' +inttostr(distance)+':'+inttostr(colmove));
            {$ENDIF}
            FSelectScroll := True;
            d.MoveBy(distance);
            d.MoveBy(-ColMove+MinInt(1,FixedCols));
            FSelectScroll := False;
          end;
        end;
      VK_PRIOR:d.MoveBy(-MaxVisibleRows);
      VK_NEXT:d.MoveBy(MaxVisibleRows);
      end;
    end;

    if (msg.CharCode = VK_DOWN) and (Row = RowCount - 1) then
    begin
      FInsLastRow := True;
    end;

    if (msg.CharCode = VK_ESCAPE) and (d.State in [dsInsert,dsEdit]) then
    begin
      d.Cancel;
    end;

    if (msg.CharCode = VK_DOWN) and (d.State = dsInsert) then
    begin
      d.Cancel;
    end;
  end;

  inherited;
end;

procedure TDBAdvStringGrid.SetHTMLTemplate(const Value: TStringList);
begin
  FHTMLTemplate.Assign(Value);
end;

procedure TDBAdvStringGrid.TemplateChanged(Sender: TObject);
var
  i: Integer;
begin
  if (csDesigning in ComponentState) and not FPageMode and (DataMapMode = dmRow) then
  begin
    for i := 1 to FHTMLTemplate.Count do
      Cells[MinInt(1,FixedCols) + i - 1,FixedRows] := FHTMLTemplate.Strings[i - 1];
    LoadFromDataSource;  
  end;

  if (csDesigning in ComponentState) and not FPageMode and (DataMapMode <> dmRow) then
  begin
    LoadFromDataSource;
  end;


end;

procedure TDBAdvStringGrid.WMSize(var Msg: TWMSize);
var
  NewRowCount: Integer;
begin
  inherited;

  if FPageMode and (DataMapMode = dmRow) then
  begin
    NewRowCount := 0;
    if (Rowcount > MaxVisibleRows) then
      NewRowCount := MaxVisibleRows;
    if (RowCount < MaxVisibleRows) and (RowCount < FMaxRows) then
      NewRowCount := MaxVisibleRows;

    if (NewRowCount <> RowCount) and (NewRowCount <> 0) then
    begin
      SetBufferCount;
      UpdateVisibleCells;
      UpdateCursorPos;
    end;
  end;

  if FPageMode and (DataMapMode = dmCell) then
  begin
    NewRowCount := 0;
    if (Rowcount > MaxVisibleRows) then
      NewRowCount := MaxVisibleRows;
    if (RowCount < MaxVisibleRows) and (RowCount < FMaxRows div (Colcount-MinInt(1,FixedCols))) then
      NewRowCount := MaxVisibleRows;

    if (NewRowCount <> RowCount) and (NewRowCount <> 0) then
    begin
      RowCount := NewRowCount;
      SetBufferCount;
      UpdateVisibleCells;
      UpdateCursorPos;
    end;
  end;
end;

procedure TDBAdvStringGrid.ReStoreColWidths;
var
  i: Integer;
begin
  FixedCols := FFixedcolumns;

  for i := 1 to ColCount do
  begin
    if (FWidths.Count >= i) then
      ColWidths[i - 1] := FWidths.Items[i - 1];
  end;
end;

procedure TDBAdvStringGrid.StoreColWidths;
var
  i: Integer;
begin
  FFixedColumns:=FixedCols;
  FWidths.Clear;
  for i := 1 to ColCount do
    FWidths.Add(ColWidths[i-1]);
end;

procedure TDBAdvStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  inherited;
  if CheckDataSet and FShowIndicator then
  begin
    if (ACol = 0) and (ARow = Row) and (FixedCols > 0) then
    begin
      Canvas.Brush.Color := clBlack;
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Width := 1;

      case DataSource.DataSet.State of
      dsInsert:
        begin
          Canvas.MoveTo(ARect.Left + 3,ARect.Top + 8);
          Canvas.LineTo(ARect.Left + 9,ARect.Top + 8);
          Canvas.MoveTo(ARect.Left + 4,ARect.Top + 6);
          Canvas.LineTo(ARect.Left + 8,ARect.Top + 11);
          Canvas.MoveTo(ARect.Left + 4,ARect.Top + 10);
          Canvas.LineTo(ARect.Left + 8,ARect.Top + 5);
        end;
      dsEdit:
        begin
          Canvas.MoveTo(ARect.Left + 5,ARect.Top + 3);
          Canvas.LineTo(ARect.Left + 5,ARect.Top + 12);
          Canvas.MoveTo(ARect.Left + 6,ARect.Top + 3);
          Canvas.LineTo(ARect.Left + 6,ARect.Top + 12);
          Canvas.MoveTo(ARect.Left + 3,ARect.Top + 2);
          Canvas.LineTo(ARect.Left + 5,ARect.Top + 2);
          Canvas.MoveTo(ARect.Left + 7,ARect.Top + 2);
          Canvas.LineTo(ARect.Left + 9,ARect.Top + 2);

          Canvas.MoveTo(ARect.Left + 3,ARect.Top + 12);
          Canvas.LineTo(ARect.Left + 5,ARect.Top + 12);
          Canvas.MoveTo(ARect.Left + 7,ARect.Top + 12);
          Canvas.LineTo(ARect.Left + 9,ARect.Top + 12);
        end;
      else
        begin
          Canvas.Polygon([Point(ARect.Left + 3,ARect.Top + 3),
            Point(ARect.Left + 3,ARect.Top + 13),Point(ARect.Left + 8,ARect.Top + 8)]);
        end;
      end;  
    end;
  end;  
end;

procedure TDBAdvStringGrid.WMVScroll(var WMScroll: TWMScroll);
var
  d: TDataSet;
begin
  if CheckDataSet and FPageMode and not FSelectScroll and KeepLinked then
  begin
    d := FDataLink.Datasource.DataSet;
    FSelectScroll := True;

    case wmscroll.ScrollCode of
    SB_LINEUP: d.MoveBy(-1);
    SB_LINEDOWN: d.MoveBy(1);
    SB_PAGEUP: d.MoveBy(-MaxVisibleRows);
    SB_PAGEDOWN: d.MoveBy(MaxVisibleRows);
    SB_THUMBPOSITION: d.MoveBy(Round(WMScroll.Pos/128 * RowCount) - Row);
    else
      inherited;
    end;
    FSelectScroll := False;
  end
  else
    inherited;
end;

procedure TDBAdvStringGrid.SetShowIndicator(const Value: boolean);
begin
  if (FShowIndicator <> Value) then
  begin
    FShowIndicator := Value;
    Invalidate;
  end;
end;

procedure TDBAdvStringGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and  (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TDBAdvStringGrid.ColumnMoved(FromIndex, ToIndex: Integer);
var
  AField: TStringGridField;
  Field: TField;
  i,cw: Integer;
  d: TDataSet;
  RealFrom, RealTo: Integer;

begin
  inherited;

  if Fields.Count > 0 then
  begin
    FFieldColMoving := true;

    RealFrom := RealColIndex(FromIndex);
    RealTo := RealColIndex(ToIndex);

    Fields.Add.Assign(Fields.Items[RealFrom - MinInt(1,FixedCols)]);
    Fields.Items[RealFrom - MinInt(1,FixedCols)].Free;

    AField := Fields.Insert(RealTo - MinInt(1,FixedCols));
    AField.Assign(Fields.Items[Fields.Count - 1]);

    Fields.Items[Fields.Count - 1].Free;

    if CheckDataSet then
    begin
      d := FDataLink.Datasource.DataSet;
      for i := 1 to Fields.Count do
      begin
        if (Fields.Items[i - 1].FFieldName <> '') then
        try
          Field := d.FieldByName(Fields.Items[i - 1].FFieldName);
          if Assigned(Field) then
            FMapping[i - 1] := Field.Index;
        finally
        end;
      end;
    end;
    FFieldColMoving := false;
  end
  else
  begin
    cw := FMapping[FromIndex - MinInt(1,FixedCols)];
    for i := FromIndex to ColCount - 2 do
      FMapping[i - MinInt(1,FixedCols)] := FMapping[i + 1 - MinInt(1,FixedCols)];
    for i := ColCount - 1 downto ToIndex + 1 do
      FMapping[i - MinInt(1,FixedCols)] := FMapping[i - 1 - MinInt(1,FixedCols)];
    FMapping[ToIndex - MinInt(1,FixedCols)] := cw;
  end;
end;

procedure TDBAdvStringGrid.GetDisplText(c, r: Integer; var Value: string);
var
  OldActive: Integer;
  Fld: TField;
  nr: Integer;
  Template: string;
  MaxRow: Integer;

begin
  if FPageMode then
  begin
    nr := SortedRowIndex(r);
    r := nr;

    if (r < FixedRows) and (r = 0) and (c >= FixedCols) and CheckDataSet then
    begin
      if (Fields.Count > c - FixedCols) then
      begin
        if Fields[c - FixedCols].Title <> '' then
        begin
          Value := Fields[c - FixedCols].Title
        end
        else
        begin
          Fld := ColumnField(c);
          if Fld <> nil then
            Value := Fld.DisplayLabel;
        end;
      end
      else
      begin
        Fld := ColumnField(c);
        if Fld <> nil then
          Value := Fld.DisplayLabel;
      end;
    end;

    MaxRow := RowCount - 1;

    if FloatingFooter.Visible and (FloatingFooter.FooterStyle = fsFixedLastRow) then
      dec(MaxRow);

    if FloatingFooter.Visible and (r = MaxRow) then
    begin
      Value := GridCells[c,r];
    end;

    if (r >= FixedRows) and (c >= FixedCols) and CheckDataSet and (r <= MaxRow)  then
    begin
      if not (FDataLink.DataSet.Bof and FDataLink.DataSet.Eof) then
      begin
        OldActive := FDataLink.ActiveRecord;

        FDataLink.ActiveRecord := r - FixedRows;

        if FHTMLTemplate.Count > c - FixedCols then
          Template := FHTMLTemplate.Strings[c - FixedCols]
        else
          Template := '';

        if Template <> '' then
        begin
          Value := HTMLDBReplace(Template,DataSource.DataSet);
        end
        else
        begin
          Fld := ColumnField(c);

          if Fld <> nil then
          begin
//            outputdebugstring(pchar(inttostr(fdatalink.activerecord)+':'+inttostr(c)+':'+fld.displaytext));

            Value := Fld.DisplayText;

            if (Fld.DataType in [ftMemo,ftFmtMemo]) and ShowMemoFields then
              Value := Fld.AsString;

            if (Fld.DataType = ftGraphic) and FShowPictureFields then
              Value := '';

            if (Fld.DataType = ftBoolean) and FShowBooleanFields then
              Value := '';
          end;
        end;

        FDataLink.ActiveRecord := OldActive;
      end;
    end;
  end;

  inherited;
end;

function TDBAdvStringGrid.ValidateCell(const NewValue: string): Boolean;
var
  Fld: TField;
  Flg: Boolean;
  FOldKeepLinked: Boolean;
  // CellValue: string;
begin
  Result := inherited ValidateCell(NewValue);

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('validate:'+newvalue+':'+cells[col,row]));
  {$ENDIF}

  if CheckDataSet and Result then
  begin
    {$IFDEF TMSDEBUG}
    DbgMsg(pchar(inttostr(col)+':'+newvalue+':'+fcurrcell));
    {$ENDIF}

    Flg := (FDataLink.DataSet.State in [dsEdit,dsInsert]);

    if Flg then
    begin
      // CellValue := GridCells[Col,Row];
      FOldKeepLinked := FKeepLinked;

      FKeepLinked := False;

      Fld := ColumnField(RealColIndex(Col));

      if Assigned(Fld) then
      begin
        if not (Fld.DataType in [ftBlob,ftMemo,ftGraphic,ftTypedBinary]) then
          Fld.Text := NewValue
        else
          Fld.AsString := NewValue;
      end;

      CurrentCell := NewValue;

      FKeepLinked := FOldKeepLinked;
    end;
  end;
end;

function TDBAdvStringGrid.ToggleCheck(ACol, ARow: Integer;
  FromEdit: Boolean): Boolean;
var
  Fld: TField;
  OldActive:  Integer;
  Flg: Boolean;
begin
  Result := False;

  if CheckDataSet then
  begin
    if FDataLink.DataSet.CanModify then
    begin
      Row := ARow;
      OldActive := FDataLink.ActiveRecord;
      FDataLink.ActiveRecord := ARow - FixedRows;

      Fld := ColumnField(RealColIndex(ACol));
      if Assigned(Fld) then
      begin
        FDataLink.DataSet.Edit;
        if Fld.DataType = ftBoolean then
        begin
          Fld.AsBoolean := not Fld.AsBoolean;
          SetCheckBoxState(ACol,ARow,Fld.AsBoolean);
        end
        else
        begin
          Flg := Fld.Value;
          Flg := not Flg;
          Fld.Value := Integer(Flg);
          SetCheckBoxState(ACol,ARow,flg);
        end;

        Result := True;
      end
      else
      begin
        GetCheckBoxState(ACol,ARow,flg);
        SetCheckBoxState(ACol,ARow,not flg);
      end;

      FDataLink.ActiveRecord := OldActive;
    end;
  end;
end;

function TDBAdvStringGrid.CanEditShow: Boolean;
var
  Fld: TField;
begin
  Result := True;

  if CheckDataSet then
  begin
    if not FDataLink.DataSet.CanModify  then
      Result := False
    else
    begin
      if (Col - FixedCols < Fields.Count) then
      begin
        Fld := ColumnField(RealColIndex(Col));
        if Assigned(Fld) then
          Result := not Fld.ReadOnly;

        if Fields[Col - FixedCols].ReadOnly then
          Result := false;   
      end;
    end;
  end;

  if Result then
    Result := inherited CanEditShow;
end;

function TDBAdvStringGrid.GetEditText(ACol, ARow: Integer): string;
var
  Fld: TField;
begin
  if CheckDataSet then
  begin
    if not (FDataLink.DataSet.State in [dsInsert,dsEdit]) then
    begin
      FDataLink.DataSet.Edit;
      FOldState := dsEdit;
    end;

    Fld := ColumnField(RealColIndex(ACol));
    if Assigned(Fld) then
    begin
      if ((Fld.DataType in [ftMemo,ftFmtMemo,ftOraCLOB]) or
         ((Fld.DataType = ftBlob) and ((Fld as TBlobField).BlobType = ftMemo))) and ShowMemoFields then

        Result := Fld.AsString
      else
        Result := Fld.Text;
    end;

    FCellCache := Result;
  end;
end;

function TDBAdvStringGrid.ColumnField(Col: Integer): TField;
begin
  Result := nil;

  if Fields.Count > 0 then
  begin
    if Col - FixedCols < Fields.Count then
    begin
      if Fields[Col - FixedCols].FieldName <> '' then
      Result := FDataLink.DataSet.Fields.FieldByName(Fields[Col - FixedCols].FieldName);
    end
  end
  else
    Result := FDataLink.DataSet.Fields[VisibleFieldIndex(FMapping[Col - FixedCols])];
end;

function TDBAdvStringGrid.GetFixedColsEx: Integer;
begin
  Result := inherited FixedCols;
end;

procedure TDBAdvStringGrid.SetFixedColsEx(const Value: Integer);
begin
  inherited FixedCols := Value;

  if CheckDataSet then
  begin
    if Fields.Count > 0 then
      ColCount := FixedCols + Fields.Count - NumHiddenColumns
    else
      ColCount := FixedCols + VisibleFieldCount - NumHiddenColumns;
  end;
end;

function TDBAdvStringGrid.GetCellType(ACol, ARow: Integer): TCellType;
var
  Fld: TField;
begin
  Result := inherited GetCellType(ACol, ARow);

  if ShowBooleanFields and (ACol >= FixedCols) and (ARow >= FixedRows) and FPageMode and CheckDataSet then
  begin
    Fld := ColumnField(RealColIndex(ACol));

    if Assigned(Fld) then
      if Fld.DataType  = ftBoolean then
        Result := ctCheckBox;
  end;
end;

function TDBAdvStringGrid.GetCellGraphic(ACol,
  ARow: Integer): TCellGraphic;
var
  Fld: TField;
  OldActive: Integer;

begin
  Result := inherited GetCellGraphic(ACol, ARow);
  if (ACol >= FixedCols) and (ARow >= FixedRows) and FPageMode and CheckDataSet then
  begin

    OldActive := FDataLink.ActiveRecord;
    FDataLink.ActiveRecord := ARow - FixedRows;

    if ShowBooleanFields then
    begin
      Fld := ColumnField(ACol);

      if Fld <> nil then
      begin
        if Fld.DataType  = ftBoolean then
        begin
          FCellGraphic.CellType := ctCheckBox;
          FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
          FCellGraphic.CellBoolean := Fld.AsBoolean;
          Result := FCellGraphic;
        end;
      end;
    end;

    if ShowPictureFields then
    begin
      Fld := ColumnField(ACol);

      if Fld <> nil then
      begin
        if Fld.DataType = ftGraphic then
        begin
          FCellGraphic.CellType := ctPicture;
          FPicture.Assign(TPicture(Fld));
          FCellGraphic.CellBitmap := TBitmap(FPicture);
          FCellGraphic.CellHAlign := haCenter;
          FCellGraphic.CellVAlign := vaCenter;
          FCellGraphic.CellTransparent := True;
          FCellGraphic.CellAngle := Integer(StretchWithAspectRatio);
          FCellGraphic.CellIndex := 4;
          Result := FCellGraphic;
        end;
      end;
    end;
    FDataLink.ActiveRecord := OldActive;
  end;
end;

procedure TDBAdvStringGrid.GetCellEditor(ACol,ARow: Integer;var AEditor:TEditorType);
begin
  if (Fields.Count > ACol - FixedCols) and (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    AEditor := Fields[ACol - FixedCols].Editor;

    ComboBox.Items.Assign(Fields[ACol - FixedCols].ComboItems);
    SpinEdit.MinValue := Fields[ACol - FixedCols].SpinMin;
    SpinEdit.MaxValue := Fields[ACol - FixedCols].SpinMax;
    SpinEdit.Increment := Fields[ACol - FixedCols].SpinStep;
    EditLink := Fields[ACol - FixedCols].EditLink;
  end;
  inherited;
end;


function TDBAdvStringGrid.GetEditMask(ACol, ARow: Integer): string;
var
  msk: string;
begin
  if (Fields.Count > ACol - FixedCols) and (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    Result := Fields[ACol - FixedCols].EditMask;
  end;

  msk := inherited GetEditMask(ACol,ARow);

  if msk <> '' then
    Result := msk;
end;


procedure TDBAdvStringGrid.GetCellColor(ACol, ARow: Integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin

  if not FFieldColMoving then
  begin
    if (Fields.Count > ACol - FixedCols) and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      if not Fields[ACol - FixedCols].ShowBands then
      begin
        ABrush.Color := Fields[ACol - FixedCols].Color;
        AFont.Assign(Fields[ACol - FixedCols].Font);
      end;
    end;
  end;
  inherited;
end;

procedure TDBAdvStringGrid.GetCellPrintColor(ACol, ARow: Integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin
  if (Fields.Count > ACol - FixedCols) and (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    ABrush.Color := Fields[ACol - FixedCols].Color;
    AFont.Assign(Fields[ACol - FixedCols].Font);
  end;
  inherited;  
end;

function TDBAdvStringGrid.GetCurrentCell: string;
begin
  Result := FCurrCell;
end;

procedure TDBAdvStringGrid.SetCurrentCell(const AValue: string);
begin
  FCurrCell := AValue;
end;

procedure TDBAdvStringGrid.SetEditText(ACol, ARow: Integer;
  const Value: string);
begin
  inherited;
  FCurrCell := Value;
end;


procedure TDBAdvStringGrid.UpdateEditingCell(ACol, ARow: Integer;
  Value: string);
begin
  FCurrCell := Value;
end;

procedure TDBAdvStringGrid.PasteInCell(ACol, ARow: Integer;
  Value: string);
var
  Fld: TField;
  OldActive: Integer;
begin
  if not CheckDataSet then
  begin
    inherited;
    Exit;
  end;

  if not FPageMode then
    inherited;

  OldActive := FDataLink.ActiveRecord;

  FDataLink.ActiveRecord := ARow - FixedRows;

  try
    if FDataLink.DataSet.State <> dsEdit then
      FDataLink.DataSet.Edit;

    Fld := ColumnField(ACol);
    if Assigned(Fld) then
      if not Fld.ReadOnly then
      begin
        if Fld.DataType = ftBoolean then
          Fld.AsBoolean := Value = CheckTrue
        else
          Fld.AsString := Value;

        // optionally post here
        // FDataLink.DataSet.Post;
        // 
      end;
  except
  end;

  FDataLink.ActiveRecord := OldActive;
end;


procedure TDBAdvStringGrid.DoDeleteRow(ARow: Integer);
begin
  if CheckDataSet then
    if not DataSource.DataSet.IsEmpty then
    begin
      DataSource.DataSet.Delete;
      if Assigned(OnAutoDeleteRow) then
        OnAutoDeleteRow(Self,ARow);
    end;    
end;

procedure TDBAdvStringGrid.DoInsertRow(ARow: Integer);
begin
  if PageMode then
  begin
//    InsertRows(ARow,1);
  end;

  if CheckDataSet then
    DataSource.DataSet.Insert;
  if Assigned(OnAutoInsertRow) then
    OnAutoInsertRow(Self,ARow);
end;

procedure TDBAdvStringGrid.SetShowBooleanFields(const Value: Boolean);
begin
  FShowBooleanFields := Value;
  Invalidate;
end;

procedure TDBAdvStringGrid.SetShowMemoFields(const Value: Boolean);
begin
  FShowMemoFields := Value;
  Invalidate;
end;

procedure TDBAdvStringGrid.SetShowPictureFields(const Value: Boolean);
begin
  FShowPictureFields := Value;
  Invalidate;
end;

function TDBAdvStringGrid.VisibleFieldCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to FDataLink.DataSet.FieldCount do
    if FDataLink.DataSet.Fields[i - 1].Visible then
      Result := Result + 1;
end;

function TDBAdvStringGrid.VisibleFieldIndex(i: Integer): Integer;
var
  j,k: Integer;
begin
  Result := 0;
  k := 0;

  for j := 1 to FDataLink.DataSet.FieldCount do
  begin
    if FDataLink.DataSet.Fields[j - 1].Visible then
      k := k + 1;

    if i = k - 1 then
    begin
      Result := j - 1;
      break;
    end;
  end;
end;


procedure TDBAdvStringGrid.RestoreCache;
begin
  FCurrCell := OriginalCellValue;
  inherited;
end;

procedure TDBAdvStringGrid.CalcFooter(ACol: Integer);
begin
  if CheckDataSet then
  begin
    if FDataLink.DataSet.State = dsBrowse then
    begin
      inherited;
    end;
  end;
end;

procedure TDBAdvStringGrid.KeyUp(var Key: Word; Shift: TShiftState);
var
  CanInsert: Boolean;
begin
  inherited;
  if CheckDataSetRW and (Key = VK_DOWN) and FInsLastRow and Navigation.AllowInsertRow then
  begin
    CanInsert := True;
    if Assigned(OnCanInsertRow) then
      OnCanInsertRow(Self,Row,CanInsert);
    if (FDataLink.DataSet.State <> dsInsert) and (FDataLink.DataSet.CanModify) and CanInsert then
      FDataLink.DataSet.Append;
  end;

  FInsLastRow := False;

  if CheckDataSet and (Key = VK_UP) and (Row = FixedRows) then
  begin
    if (FDataLink.DataSet.State in [dsInsert,dsEdit]) then
      FDataLink.DataSet.Post;
  end;
end;

function TDBAdvStringGrid.GetHTMLTemplate: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to HTMLTemplate.Count do
    Result := Result + HTMLTemplate.Strings[i - 1];
end;

procedure TDBAdvStringGrid.QueryAddRow(var AllowAdd: Boolean);
begin
  inherited QueryAddRow(AllowAdd);
  if AllowAdd and CheckDataSet then
    DataSource.DataSet.Append;
  AllowAdd := False;
  Col := FixedCols;
end;

procedure TDBAdvStringGrid.QueryInsertRow(ARow: Integer;
  var AllowInsert: Boolean);
begin
  inherited QueryInsertRow(ARow, AllowInsert);

  if AllowInsert and  CheckDataSet then
    DataSource.DataSet.Insert;
end;

procedure TDBAdvStringGrid.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

function TDBAdvStringGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBAdvStringGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;




end.
