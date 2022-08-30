{***************************************************************************}
{ TDBADVLISTVIEW component                                                  }
{ for Delphi & C++Builder                                                   }
{ version 1.6                                                               }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   copyright © 1998-2003                                                   }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit DBAdvLst;

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Grids, DB,
  Graphics, AdvListv, ComCtrls, StdCtrls;

const
  MAX_FIELDS = 255;

type
  EDBAdvListviewError = class(Exception);

  TDBAdvListView = class;

  TColWidthArray = array[0..MAX_FIELDS] of integer;

  TListViewField = class(TCollectionItem)
  private
    FFieldName: string;
    FTitle: string;
    FColor: TColor;
    FHighLight: TColor;
    FFont: TFont;
    procedure SetTitle(const value:string);
    procedure SetColor(const value:tcolor);
    procedure SetFont(const value:tFont);
    procedure FontChanged(Sender: TObject);
    procedure SetFieldName(const value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName:string read fFieldName write SetFieldName;
    property Title:string read fTitle write SetTitle;
    property Color:TColor read fColor write SetColor;
    property HighLight:TColor read fHighLight write fHighLight;
    property Font:TFont read fFont write SetFont;
  end;

  TListViewFields = class(TCollection)
  private
    FOwner : TDBAdvListView;
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure UpdateListView;
    procedure UpdateListViewData;
  public
    constructor Create(AOwner:TDBAdvListView);
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TListViewField;
    procedure SetItem(Index: Integer; const Value: TListViewField);
    function Add:TListViewField;
    function Insert(Index: Integer): TListViewField;
    property Items[Index: Integer]: TListViewField read GetItem write SetItem;
  end;

  TAdvListViewDataLink = class(TDataLink)
  private
    FListView:TDBAdvListView;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(distance:integer); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AListView: TDBAdvListView);
    destructor Destroy; override;
  end;

  {TDBAdvListView}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvListView = class(TAdvListView)
  private
    FDataLink: TAdvListViewDataLink;
    FDataScroll: Boolean;
    FListViewFields: TListViewFields;
    FCloseWhenLoaded: Boolean;
    FColWidths: TColWidthArray;
    function GetDataSource:TDataSource;
    procedure SetDatasource(Value: TDatasource);
    procedure SetCloseWhenLoaded(Value: Boolean);
    procedure SetListViewFields(value: TListViewFields);
    procedure SaveColWidths;
    procedure LoadColWidths;
    procedure ClearColWidths;
    function CheckDataSet: Boolean;
    function CheckDataSetAttached: Boolean;
  protected
    procedure UpdateCell(i: Integer);
    procedure Loaded; override;
    procedure QueryDrawProp(Item,Subitem: Integer; AState:TOwnerDrawState;
                            ABrush:TBrush; AFont:TFont; ItemText:string); override;
    property CloseWhenLoaded: Boolean read FCloseWhenLoaded write SetCloseWhenLoaded;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SelectionChanged(iItem:integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromDataSource;
  published
    property Datasource: TDatasource read GetDatasource write SetDatasource;
    property DataScroll: Boolean read FDataScroll write FDataScroll;
    property Fields: TListViewFields read FListViewFields write SetListViewFields;
  end;


implementation

{$IFDEF TMSDEBUG}
uses
 Dialogs;
{$ENDIF}

procedure dbg(p:string);
begin
  {$IFDEF TMSDEBUG}
  ShowMessage(p);
  OutputDebugString(pchar(p));
  {$ENDIF}
end;

function TDBAdvListView.CheckDataSet: Boolean;
begin
  Result := False;
  if not Assigned(FDataLink) then Exit;
  if not Assigned(FDataLink.DataSource) then Exit;
  if Assigned(FDataLink.Datasource.DataSet) then
  begin
    if FDataLink.Datasource.DataSet.Active then Result := True;
  end;
end;

function TDBAdvListView.CheckDataSetAttached: Boolean;
begin
  Result := False;
  if not Assigned(FDataLink) then Exit;
  if not Assigned(FDataLink.DataSource) then Exit;
  if Assigned(FDataLink.Datasource.DataSet) then
  begin
    Result := True;
  end;
end;


procedure TDBAdvListView.UpdateCell(i:integer);
var
  d: TDataSet;
  j : Integer;
  dt : TFieldType;
  lis: TListItem;
  mapped: Boolean;
  field: TField;
  fieldidx: Integer;
  mapping: array[0..MAX_FIELDS] of Integer;

begin
  if not CheckDataSet then Exit;
  if (i < 0) or (i > Items.Count) then Exit;

  d := FDataLink.Datasource.DataSet;

  if d.FieldCount = 0 then Exit;

  dbg(pchar('record change : '+inttostr(i)));

  if (i >= Items.Count) then
    lis := Items.Add
  else
    lis := Items[i];

  dbg(pchar(lis.Caption));

  for i := 0 to MAX_FIELDS do
    Mapping[i] := -1;

  for i := 1 to Fields.Count do
  begin
    if (Fields.Items[i-1] as TListViewField).FieldName<>'' then
    try
      Field := d.FieldByName( (self.Fields.Items[i-1] as TListViewField).FieldName);
      if Assigned(Field) then mapping[i-1] := Field.Index;
    finally
    end;
  end;

  {if  (self.Fields.Count>0) then numfields:=self.Fields.Count else numfields:=d.FieldCount;}
  mapped := Fields.Count > 0;

  with d do
  for j := 0 to FieldCount-1 do
  begin
    if Mapped then FieldIdx := Mapping[j] else FieldIdx := j;

    if FieldIdx = -1 then Continue;

    //dbg(pchar('field name = '+fields[j].DisplayName+' - '+inttostr(j)));

    if (FieldIdx >= 0) and (FieldIdx < FieldCount) then
    begin
      dt := Fields[FieldIdx].DataType;
      if not (dt in [ftBlob,ftGraphic,ftTypedBinary]) then
      begin
        if j = 0 then
          lis.Caption := Trim(Fields[FieldIdx].DisplayText)
        else
          lis.SubItems[j-1] := Trim(Fields[FieldIdx].DisplayText);
      end;  // if (dt <> ftBlob) and ...
      if dt = ftMemo then
      begin
        if j = 0 then
          lis.Caption := Trim(Fields[FieldIdx].AsString)
        else
          lis.SubItems[j-1] := Trim(Fields[FieldIdx].AsString);
      end;
    end;
  end;
end;

procedure TDBAdvListView.LoadFromDataSource;
var
  d: TDataSet;
  i: Integer;
  dt : TFieldType;
  cb : TBookMark;
  lis: TListitem;
  lic: TListcolumn;
  mapping: array[0..MAX_FIELDS] of Integer;
  field: TField;
  mapped: Boolean;
  NumFields: Integer;
  FieldIdx: Integer;
  NumRecords, VisibleRecords: Integer;
  NumTextFields: Integer;

begin
  if csLoading in ComponentState then Exit;

  if not CheckDataSet then Exit;

  dbg('loadfromdatasource');
  {Remove all old rows and Columns}

  Items.Clear;

  for i:=1 to Columns.Count do Columns[i-1].Caption:='';

  {Check to see if Dataset is Open}
  d := FDataLink.Datasource.DataSet;

  if (d <> nil) then
  begin
   dbg('dataset assigned');

//   if not d.Active then
//   Raise error if not. is normal, no error required
//   raise EAdvListViewError.Create('Dataset ' + TDataSet(d).Name + ' not Active')
//   else
   if d.Active then
     with d do
     begin
       dbg('starting the load');

       DisableControls;

       cb := d.GetBookmark;

       for i := 0 to MAX_FIELDS do mapping[i] := -1;

       for i := 0 to Self.Fields.Count - 1 do
       begin
         if (Self.Fields.Items[i].FieldName <> '') then
         try
           Field := d.FieldByName(Self.Fields.Items[i].FieldName);
           if Assigned(Field) then
             mapping[i] := Field.Index;
         finally
         end;
       end;

      dbg('mapping done');

      First;

      NumTextFields := 0;

      for i := 0  to FieldCount - 1 do
      begin
        if not (Fields[i].DataType in [ftBlob,ftTypedBinary,ftGraphic]) then
          Inc(NumTextFields);
      end;

      if Self.Fields.Count > 0 then
        NumFields := Self.Fields.Count
      else
        NumFields := NumTextFields;

      if DetailView.Visible then Dec(NumFields);

      Mapped := Self.Fields.Count > 0;

      for i := 1 to NumFields do
      begin
        if (Self.Columns.Count<i) then
          lic := Columns.Add
        else
          lic := Columns[i-1];

        if Mapped then
          Fieldidx := Mapping[i-1]
        else
          FieldIdx := i - 1;

        if FieldIdx = -1 then Continue;

        if (FieldIdx >= 0) and (FieldIdx < FieldCount) then
        begin
          dt := Fields[Fieldidx].DataType;
          if dt in [ftSmallint,ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency , ftLargeint ] then
            lic.Alignment:=taRightJustify;

          if Mapped then
          begin
            if (self.Fields.Items[i-1].Title <> '') then
               lic.Caption := self.Fields.Items[i-1].Title;
          end
          else
          begin
            if Length(TField(Fields[fieldidx]).DisplayLabel) > 0 then
            begin
              dt := Fields[Fieldidx].DataType;
              if not (dt in [ftBlob, ftGraphic, ftTypedBinary]) then
                lic.Caption := TField(Fields[Fieldidx]).DisplayLabel;
            end
            else
            begin
              if dt in [ftBlob,ftGraphic,ftTypedBinary] then
                lic.Caption := TField(Fields[FieldIdx]).DisplayName;
            end;
          end;
        end;
      end;  // for i := 0 to FieldCount-1

      // iterate through records and assign to rows

      if DetailView.Visible then Inc(NumFields);      

      VisibleRecords := VisibleItems;
      Numrecords := 0;

      while (not Eof) do
      begin
        if (csDesigning in ComponentState) and
           (Numrecords > VisibleRecords) then Break;

        inc(Numrecords);
        lis := Items.Add;
        lis.Imageindex := -1;

        // Now iterate through the fields aray and place in cells
        for i := 1 to NumFields do
        begin
          if Mapped then
            Fieldidx := Mapping[i-1]
          else
            FieldIdx := i-1;

          if FieldIdx = -1 then Continue;

          dt := Fields[FieldIdx].DataType;

          if not (dt in [ftBlob, ftGraphic, ftTypedBinary,ftMemo]) then
          begin
            if (i = 1) then
              lis.Caption := Trim(Fields[FieldIdx].DisplayText)
            else
              lis.Subitems.Add(Trim(Fields[fieldidx].DisplayText));
          end;
          
          if (dt = ftMemo) then
          begin
            if (i = 1) then
              lis.caption := Trim(Fields[FieldIdx].AsString)
            else
              lis.Subitems.Add(Trim(Fields[fieldidx].AsString));
          end;

        end;
        Next;
      end;  // while not Eof

      if (FCloseWhenLoaded) and not (csDesigning in ComponentState) then Close;

      EnableControls;

      d.GotoBookMark(cb);
      FreeBookmark(cb);

    end;  // with d do
  end;  // if d <> nil
end;

procedure TDBAdvListView.Loaded;
begin
 inherited Loaded;
 LoadFromDatasource;
end;

procedure TDBAdvListView.QueryDrawProp(item,subitem:integer;aState:TOwnerDrawState;
                          ABrush:TBrush;aFont:TFont;itemtext:string);
begin
  inc(Subitem);
  if (Subitem<Fields.Count) and (Subitem>=0) then
  begin
    ABrush.Color:=(Fields.Items[Subitem] as TListViewField).Color;

    if Assigned((self.Fields.Items[subitem] as TListViewField).Font) then
      AFont.Assign((self.Fields.Items[subitem] as TListViewField).Font);

    if (odSelected in aState) then
      AFont.Color:=(self.Fields.Items[subitem] as TListViewField).HighLight;
  end;

  Dec(subitem);

  if Assigned(OnDrawItemProp) then
    OnDrawItemProp(Self,Item,SubItem,AState,ABrush,AFont,ItemText);
end;

procedure TDBAdvListView.SetDataSource(Value : TDatasource);
begin
  if (Value=nil) then Items.Clear;

  if (FDataLink.DataSource <> Value) then
  begin
     FDataLink.DataSource := Value;
     if (Value <> nil) then LoadFromDataSource;
  end;
end;

function TDBAdvListView.GetDataSource: TDataSource;
begin
  dbg('in get datasource');
  Result := FDataLink.DataSource;
end;

procedure TDBAdvListView.SetListViewFields(value:tListViewFields);
begin
  FListViewFields.Assign(value);
end;

procedure TDBAdvListView.SetCloseWhenLoaded(Value: Boolean);
begin
  if (FCloseWhenLoaded <> Value) then FCloseWhenLoaded := Value;
end;

procedure TDBAdvListView.LoadColWidths;
var
  i: Integer;
begin
  for i := 1 to Columns.Count do
  begin
    if FColWidths[i-1] <> -1 then Columns[i-1].Width := FColWidths[i-1];
  end;
end;

procedure TDBAdvListView.SaveColWidths;
var
  i: Integer;
begin
  ClearColWidths;
  for i := 1 to Columns.Count do
  begin
    FColWidths[i-1] := Columns[i-1].Width;
  end;
end;

procedure TDBAdvListView.ClearColWidths;
var
  i: Integer;
begin
  for i := 0 to MAX_FIELDS do FColWidths[i] := -1;
end;

constructor TDBAdvListView.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FDataLink := TAdvListViewDataLink.Create(Self);
  FListViewFields := TListViewFields.Create(Self);
  ClearColWidths;
  dbg('in create method');
end;

destructor TDBAdvListView.Destroy;
begin
  FDataLink.Free;
  FListViewFields.Free;
  inherited Destroy;
end;

{ FListViewDataLink }

procedure TAdvListViewDataLink.ActiveChanged;
begin
  inherited ActiveChanged;

  dbg('in datalink activechanged');

  if not FListView.CheckDataSetAttached then
  begin
    FListView.SaveColWidths;
    FListView.Items.Clear;
    FListView.Columns.Clear;
    FListView.Fields.Clear;
    Exit;
  end;

  if not Assigned(FListView.DataSource) then Exit;

  if not FListView.Datasource.Dataset.Active then
  begin
    FListView.SaveColWidths;
    FListView.Items.Clear;
    FListView.Columns.Clear;
  end
  else
  begin
    FListView.LoadFromDatasource;
    FListView.LoadColWidths;
  end;

end;

constructor TAdvListViewDataLink.Create(AListView: TDBAdvListView);
begin
  inherited Create;
  FListView := AListView;
end;

procedure TAdvListViewDataLink.DataSetChanged;
begin
  dbg('in datalink datasetchanged');
  // FListView.loadfromdatasource;
  inherited DataSetChanged;
end;

procedure TAdvListViewDataLink.DataSetScrolled(distance:integer);
begin
  dbg('in datalink datasetscrolled');
  if not Assigned(Datasource) then Exit;

  if FListView.FDataScroll then
    FListView.SelectItem(self.DataSource.DataSet.RecNo-1);

  inherited DataSetScrolled(Distance);
end;

procedure TAdvListViewDataLink.RecordChanged(Field: TField);
begin
  dbg('in recordchanged');
  inherited RecordChanged(Field);
end;

destructor TAdvListViewDataLink.Destroy;
begin
  inherited Destroy;
end;

procedure TAdvListViewDataLink.UpdateData;
var
  RecNo: Integer;
begin
  if (DataSource.DataSet.RecNo<>-1) then
    RecNo := DataSource.DataSet.RecNo-1
  else
    RecNo := DataSource.DataSet.RecNo;

  dbg(pchar('in update data :'+inttostr(RecNo)));
  inherited UpdateData;
  FListView.UpdateCell(RecNo);
end;

{ TListViewFields }

function TListViewFields.Add: TListViewField;
begin
  Result := TListViewField(inherited Add);
end;

constructor TListViewFields.Create(AOwner: TDBAdvListView);
begin
  inherited Create(TListViewField);
  FOwner := AOwner;
end;

function TListViewFields.GetItem(Index: Integer): TListViewField;
begin
  Result := TListViewField(inherited Items[Index]);
end;

function TListViewFields.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TListViewFields.Insert(Index: Integer): TListViewField;
begin
  Result := TListViewField(inherited Insert(Index));
end;

procedure TListViewFields.SetItem(Index: Integer;
  const Value: TListViewField);
begin

end;

procedure TListViewFields.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  UpdateListViewData;
end;

procedure TListViewFields.UpdateListView;
var
  DBListView:TDBAdvListView;
begin
  DBListView := GetOwner as TDBAdvListView;
  if not Assigned(DBListView) then Exit;
  DBListView.Invalidate;
end;

procedure TListViewFields.UpdateListViewData;
var
  DBListView:TDBAdvListView;
begin
  DBListView := GetOwner as TDBAdvListView;
  if not Assigned(DBListView) then Exit;
  with (DBListView) do
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
       LoadFromDataSource;
  end;
end;

{ TListViewField }
constructor TListViewField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFont:=TFont.Create;
  FHighLight:=clHighLightText;

  if Assigned(((Collection as TListViewFields).GetOwner)) then
  begin
    FFont.Assign(((Collection as TListViewFields).GetOwner as TDBAdvListView).Font);
    FColor:=((Collection as TListViewFields).GetOwner as TDBAdvListView).Color;
    FFont.OnChange:=FontChanged;
  end;
end;

destructor TListViewField.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TListViewField.Assign(Source: TPersistent);
begin
  if Source is TListViewField then
  begin
    FFont.Assign(TListViewField(Source).Font);
    FColor := TListViewField(Source).Color;
    FHighLight := TListViewField(Source).HighLight;
    FTitle := TListViewField(Source).Title;
    FFieldName := TListViewField(Source).FieldName;
  end;
end;


function TListViewField.GetDisplayName: string;
begin
  Result := FFieldName;
end;

procedure TListViewField.SetColor(const Value: TColor);
begin
  FColor := Value;
  (Collection as TListViewFields).UpdateListView;
end;

procedure TListViewField.SetFieldName(const Value: string);
begin
  FFieldName := Value;
  (Collection as TListViewFields).UpdateListViewData;
end;

procedure TListViewField.SetFont(const Value: TFont);
begin
  if Assigned(Value) then
  begin
    FFont.Assign(value);
    (Collection as TListViewFields).UpdateListView;
  end;
end;

procedure TListViewField.SetTitle(const Value: string);
var
  DBListView:TDBAdvListView;
begin
  FTitle := Value;
  DBListView := (Collection as TListViewFields).GetOwner as TDBAdvListView;
  if not Assigned(DBListView) then Exit;
  if (Index >= DBListView.Columns.Count) then Exit;
  DBListView.Columns.Items[Index].Caption := Value;
end;

procedure TListViewField.FontChanged(Sender: TObject);
begin
  (Collection as TListViewFields).UpdateListView;
end;

procedure TDBAdvListView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    begin
      DataSource := nil;
      Items.Clear;
      Fields.Clear;
    end;
end;

procedure TDBAdvListView.SelectionChanged(iItem:integer);
begin
  inherited;
  if not CheckDataSet then Exit;
  if (iItem=-1) then Exit;
  with FDataLink.DataSource.DataSet do
  begin
    if FDataScroll then MoveBy(iItem-RecNo+1);
  end;
end;

end.



